/* read-rtl-function.c - Reader for RTL function dumps
   Copyright (C) 2016-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "diagnostic.h"
#include "read-md.h"
#include "rtl.h"
#include "cfghooks.h"
#include "stringpool.h"
#include "function.h"
#include "tree-cfg.h"
#include "cfg.h"
#include "basic-block.h"
#include "cfgrtl.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "toplev.h"
#include "varasm.h"
#include "read-rtl-function.h"
#include "selftest.h"
#include "selftest-rtl.h"
#include "regs.h"
#include "function-abi.h"

/* Forward decls.  */
class function_reader;
class fixup;

/* Edges are recorded when parsing the "insn-chain" directive,
   and created at the end when all the blocks ought to exist.
   This struct records an "edge-from" or "edge-to" directive seen
   at LOC, which will be turned into an actual CFG edge once
   the "insn-chain" is fully parsed.  */

class deferred_edge
{
public:
  deferred_edge (file_location loc, int src_bb_idx, int dest_bb_idx, int flags)
  : m_loc (loc), m_src_bb_idx (src_bb_idx), m_dest_bb_idx (dest_bb_idx),
    m_flags (flags)
  {}

  file_location m_loc;
  int m_src_bb_idx;
  int m_dest_bb_idx;
  int m_flags;
};

/* Subclass of rtx_reader for reading function dumps.  */

class function_reader : public rtx_reader
{
 public:
  function_reader ();
  ~function_reader ();

  /* Overridden vfuncs of class md_reader.  */
  void handle_unknown_directive (file_location, const char *) FINAL OVERRIDE;

  /* Overridden vfuncs of class rtx_reader.  */
  rtx read_rtx_operand (rtx x, int idx) FINAL OVERRIDE;
  void handle_any_trailing_information (rtx x) FINAL OVERRIDE;
  rtx postprocess (rtx) FINAL OVERRIDE;
  const char *finalize_string (char *stringbuf) FINAL OVERRIDE;

  rtx_insn **get_insn_by_uid (int uid);
  tree parse_mem_expr (const char *desc);

 private:
  void parse_function ();
  void create_function ();
  void parse_param ();
  void parse_insn_chain ();
  void parse_block ();
  int parse_bb_idx ();
  void parse_edge (basic_block block, bool from);
  rtx_insn *parse_insn (file_location loc, const char *name);
  void parse_cfg (file_location loc);
  void parse_crtl (file_location loc);
  void create_edges ();

  int parse_enum_value (int num_values, const char *const *strings);

  void read_rtx_operand_u (rtx x, int idx);
  void read_rtx_operand_i_or_n (rtx x, int idx, char format_char);
  rtx read_rtx_operand_r (rtx x);
  rtx extra_parsing_for_operand_code_0 (rtx x, int idx);

  void add_fixup_insn_uid (file_location loc, rtx insn, int operand_idx,
			   int insn_uid);

  void add_fixup_note_insn_basic_block (file_location loc, rtx insn,
					int operand_idx, int bb_idx);

  void add_fixup_source_location (file_location loc, rtx_insn *insn,
				  const char *filename, int lineno, int colno);

  void add_fixup_expr (file_location loc, rtx x,
		       const char *desc);

  rtx consolidate_singletons (rtx x);
  rtx parse_rtx ();
  void maybe_read_location (rtx_insn *insn);

  void handle_insn_uids ();
  void apply_fixups ();

 private:
  struct uid_hash : int_hash <int, -1, -2> {};
  hash_map<uid_hash, rtx_insn *> m_insns_by_uid;
  auto_vec<fixup *> m_fixups;
  rtx_insn *m_first_insn;
  auto_vec<tree> m_fake_scope;
  char *m_name;
  bool m_have_crtl_directive;
  basic_block m_bb_to_insert_after;
  auto_vec <deferred_edge> m_deferred_edges;
  int m_highest_bb_idx;
};

/* Abstract base class for recording post-processing steps that must be
   done after reading a .rtl file.  */

class fixup
{
 public:
  /* Constructor for a fixup at LOC affecting X.  */
  fixup (file_location loc, rtx x)
    : m_loc (loc), m_rtx (x)
  {}
  virtual ~fixup () {}

  virtual void apply (function_reader *reader) const = 0;

 protected:
  file_location m_loc;
  rtx m_rtx;
};

/* An abstract subclass of fixup for post-processing steps that
   act on a specific operand of a specific instruction.  */

class operand_fixup : public fixup
{
 public:
  /* Constructor for a fixup at LOC affecting INSN's operand
     with index OPERAND_IDX.  */
  operand_fixup (file_location loc, rtx insn, int operand_idx)
    : fixup (loc, insn), m_operand_idx (operand_idx)
  {}

 protected:
  int m_operand_idx;
};

/* A concrete subclass of operand_fixup: fixup an rtx_insn *
   field based on an integer UID.  */

class fixup_insn_uid : public operand_fixup
{
 public:
  /* Constructor for a fixup at LOC affecting INSN's operand
     with index OPERAND_IDX.  Record INSN_UID as the uid.  */
  fixup_insn_uid (file_location loc, rtx insn, int operand_idx, int insn_uid)
    : operand_fixup (loc, insn, operand_idx),
      m_insn_uid (insn_uid)
  {}

  void apply (function_reader *reader) const;

 private:
  int m_insn_uid;
};

/* A concrete subclass of operand_fixup: fix up a
   NOTE_INSN_BASIC_BLOCK based on an integer block ID.  */

class fixup_note_insn_basic_block : public operand_fixup
{
 public:
  fixup_note_insn_basic_block (file_location loc, rtx insn, int operand_idx,
			       int bb_idx)
    : operand_fixup (loc, insn, operand_idx),
      m_bb_idx (bb_idx)
  {}

  void apply (function_reader *reader) const;

 private:
  int m_bb_idx;
};

/* A concrete subclass of fixup (not operand_fixup): fix up
   the expr of an rtx (REG or MEM) based on a textual dump.  */

class fixup_expr : public fixup
{
 public:
  fixup_expr (file_location loc, rtx x, const char *desc)
    : fixup (loc, x),
      m_desc (xstrdup (desc))
  {}

  ~fixup_expr () { free (m_desc); }

  void apply (function_reader *reader) const;

 private:
  char *m_desc;
};

/* Return a textual description of the operand of INSN with
   index OPERAND_IDX.  */

static const char *
get_operand_name (rtx insn, int operand_idx)
{
  gcc_assert (is_a <rtx_insn *> (insn));
  switch (operand_idx)
    {
    case 0:
      return "PREV_INSN";
    case 1:
      return "NEXT_INSN";
    default:
      return NULL;
    }
}

/* Fixup an rtx_insn * field based on an integer UID, as read by READER.  */

void
fixup_insn_uid::apply (function_reader *reader) const
{
  rtx_insn **insn_from_uid = reader->get_insn_by_uid (m_insn_uid);
  if (insn_from_uid)
    XEXP (m_rtx, m_operand_idx) = *insn_from_uid;
  else
    {
      const char *op_name = get_operand_name (m_rtx, m_operand_idx);
      if (op_name)
	error_at (m_loc,
		  "insn with UID %i not found for operand %i (`%s') of insn %i",
		  m_insn_uid, m_operand_idx, op_name, INSN_UID (m_rtx));
      else
	error_at (m_loc,
		  "insn with UID %i not found for operand %i of insn %i",
		  m_insn_uid, m_operand_idx, INSN_UID (m_rtx));
    }
}

/* Fix up a NOTE_INSN_BASIC_BLOCK based on an integer block ID.  */

void
fixup_note_insn_basic_block::apply (function_reader *) const
{
  basic_block bb = BASIC_BLOCK_FOR_FN (cfun, m_bb_idx);
  gcc_assert (bb);
  NOTE_BASIC_BLOCK (m_rtx) = bb;
}

/* Fix up the expr of an rtx (REG or MEM) based on a textual dump
   read by READER.  */

void
fixup_expr::apply (function_reader *reader) const
{
  tree expr = reader->parse_mem_expr (m_desc);
  switch (GET_CODE (m_rtx))
    {
    case REG:
      set_reg_attrs_for_decl_rtl (expr, m_rtx);
      break;
    case MEM:
      set_mem_expr (m_rtx, expr);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Strip trailing whitespace from DESC.  */

static void
strip_trailing_whitespace (char *desc)
{
  char *terminator = desc + strlen (desc);
  while (desc < terminator)
    {
      terminator--;
      if (ISSPACE (*terminator))
	*terminator = '\0';
      else
	break;
    }
}

/* Return the numeric value n for GET_NOTE_INSN_NAME (n) for STRING,
   or fail if STRING isn't recognized.  */

static int
parse_note_insn_name (const char *string)
{
  for (int i = 0; i < NOTE_INSN_MAX; i++)
    if (strcmp (string, GET_NOTE_INSN_NAME (i)) == 0)
      return i;
  fatal_with_file_and_line ("unrecognized NOTE_INSN name: `%s'", string);
}

/* Return the register number for NAME, or return -1 if it isn't
   recognized.  */

static int
lookup_reg_by_dump_name (const char *name)
{
  for (int i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (reg_names[i][0]
	&& ! strcmp (name, reg_names[i]))
      return i;

  /* Also lookup virtuals.  */
  if (!strcmp (name, "virtual-incoming-args"))
    return VIRTUAL_INCOMING_ARGS_REGNUM;
  if (!strcmp (name, "virtual-stack-vars"))
    return VIRTUAL_STACK_VARS_REGNUM;
  if (!strcmp (name, "virtual-stack-dynamic"))
    return VIRTUAL_STACK_DYNAMIC_REGNUM;
  if (!strcmp (name, "virtual-outgoing-args"))
    return VIRTUAL_OUTGOING_ARGS_REGNUM;
  if (!strcmp (name, "virtual-cfa"))
    return VIRTUAL_CFA_REGNUM;
  if (!strcmp (name, "virtual-preferred-stack-boundary"))
    return VIRTUAL_PREFERRED_STACK_BOUNDARY_REGNUM;
  /* TODO: handle "virtual-reg-%d".  */

  /* In compact mode, pseudos are printed with '< and '>' wrapping the regno,
     offseting it by (LAST_VIRTUAL_REGISTER + 1), so that the
     first non-virtual pseudo is dumped as "<0>".  */
  if (name[0] == '<' && name[strlen (name) - 1] == '>')
    {
      int dump_num = atoi (name + 1);
      return dump_num + LAST_VIRTUAL_REGISTER + 1;
    }

  /* Not found.  */
  return -1;
}

/* class function_reader : public rtx_reader */

/* function_reader's constructor.  */

function_reader::function_reader ()
: rtx_reader (true),
  m_first_insn (NULL),
  m_name (NULL),
  m_have_crtl_directive (false),
  m_bb_to_insert_after (NULL),
  m_highest_bb_idx (EXIT_BLOCK)
{
}

/* function_reader's destructor.  */

function_reader::~function_reader ()
{
  int i;
  fixup *f;
  FOR_EACH_VEC_ELT (m_fixups, i, f)
    delete f;

  free (m_name);
}

/* Implementation of rtx_reader::handle_unknown_directive,
   for parsing the remainder of a directive with name NAME
   seen at START_LOC.

   Require a top-level "function" directive, as emitted by
   print_rtx_function, and parse it.  */

void
function_reader::handle_unknown_directive (file_location start_loc,
					   const char *name)
{
  if (strcmp (name, "function"))
    fatal_at (start_loc, "expected 'function'");

  if (flag_lto)
    error ("%<__RTL%> function cannot be compiled with %<-flto%>");

  parse_function ();
}

/* Parse the output of print_rtx_function (or hand-written data in the
   same format), having already parsed the "(function" heading, and
   finishing immediately before the final ")".

   The "param" and "crtl" clauses are optional.  */

void
function_reader::parse_function ()
{
  m_name = xstrdup (read_string (0));

  create_function ();

  while (1)
    {
      int c = read_skip_spaces ();
      if (c == ')')
	{
	  unread_char (c);
	  break;
	}
      unread_char (c);
      require_char ('(');
      file_location loc = get_current_location ();
      struct md_name directive;
      read_name (&directive);
      if (strcmp (directive.string, "param") == 0)
	parse_param ();
      else if (strcmp (directive.string, "insn-chain") == 0)
	parse_insn_chain ();
      else if (strcmp (directive.string, "crtl") == 0)
	parse_crtl (loc);
      else
	fatal_with_file_and_line ("unrecognized directive: %s",
				  directive.string);
    }

  handle_insn_uids ();

  apply_fixups ();

  /* Rebuild the JUMP_LABEL field of any JUMP_INSNs in the chain, and the
     LABEL_NUSES of any CODE_LABELs.

     This has to happen after apply_fixups, since only after then do
     LABEL_REFs have their label_ref_label set up.  */
  rebuild_jump_labels (get_insns ());

  crtl->init_stack_alignment ();
}

/* Set up state for the function *before* fixups are applied.

   Create "cfun" and a decl for the function.
   By default, every function decl is hardcoded as
      int test_1 (int i, int j, int k);
   Set up various other state:
   - the cfg and basic blocks (edges are created later, *after* fixups
   are applied).
   - add the function to the callgraph.  */

void
function_reader::create_function ()
{
  /* We start in cfgrtl mode, rather than cfglayout mode.  */
  rtl_register_cfg_hooks ();

  /* When run from selftests or "rtl1", cfun is NULL.
     When run from "cc1" for a C function tagged with __RTL, cfun is the
     tagged function.  */
  if (!cfun)
    {
      tree fn_name = get_identifier (m_name ? m_name : "test_1");
      tree int_type = integer_type_node;
      tree return_type = int_type;
      tree arg_types[3] = {int_type, int_type, int_type};
      tree fn_type = build_function_type_array (return_type, 3, arg_types);
      tree fndecl = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL, fn_name, fn_type);
      tree resdecl = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE,
				 return_type);
      DECL_ARTIFICIAL (resdecl) = 1;
      DECL_IGNORED_P (resdecl) = 1;
      DECL_RESULT (fndecl) = resdecl;
      allocate_struct_function (fndecl, false);
      /* This sets cfun.  */
      current_function_decl = fndecl;
    }

  gcc_assert (cfun);
  gcc_assert (current_function_decl);
  tree fndecl = current_function_decl;

  /* Mark this function as being specified as __RTL.  */
  cfun->curr_properties |= PROP_rtl;

  /* cc1 normally inits DECL_INITIAL (fndecl) to be error_mark_node.
     Create a dummy block for it.  */
  DECL_INITIAL (fndecl) = make_node (BLOCK);

  cfun->curr_properties = (PROP_cfg | PROP_rtl);

  /* Do we need this to force cgraphunit.c to output the function? */
  DECL_EXTERNAL (fndecl) = 0;
  DECL_PRESERVE_P (fndecl) = 1;

  /* Add to cgraph.  */
  cgraph_node::finalize_function (fndecl, false);

  /* Create bare-bones cfg.  This creates the entry and exit blocks.  */
  init_empty_tree_cfg_for_function (cfun);
  ENTRY_BLOCK_PTR_FOR_FN (cfun)->flags |= BB_RTL;
  EXIT_BLOCK_PTR_FOR_FN (cfun)->flags |= BB_RTL;
  init_rtl_bb_info (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  init_rtl_bb_info (EXIT_BLOCK_PTR_FOR_FN (cfun));
  m_bb_to_insert_after = ENTRY_BLOCK_PTR_FOR_FN (cfun);

}

/* Look within the params of FNDECL for a param named NAME.
   Return NULL_TREE if one isn't found.  */

static tree
find_param_by_name (tree fndecl, const char *name)
{
  for (tree arg = DECL_ARGUMENTS (fndecl); arg; arg = TREE_CHAIN (arg))
    if (id_equal (DECL_NAME (arg), name))
      return arg;
  return NULL_TREE;
}

/* Parse the content of a "param" directive, having already parsed the
   "(param".  Consume the trailing ')'.  */

void
function_reader::parse_param ()
{
  require_char_ws ('"');
  file_location loc = get_current_location ();
  char *name = read_quoted_string ();

  /* Lookup param by name.  */
  tree t_param = find_param_by_name (cfun->decl, name);
  if (!t_param)
    fatal_at (loc, "param not found: %s", name);

  /* Parse DECL_RTL.  */
  require_char_ws ('(');
  require_word_ws ("DECL_RTL");
  DECL_WRTL_CHECK (t_param)->decl_with_rtl.rtl = parse_rtx ();
  require_char_ws (')');

  /* Parse DECL_RTL_INCOMING.  */
  require_char_ws ('(');
  require_word_ws ("DECL_RTL_INCOMING");
  DECL_INCOMING_RTL (t_param) = parse_rtx ();
  require_char_ws (')');

  require_char_ws (')');
}

/* Parse zero or more child insn elements within an
   "insn-chain" element.  Consume the trailing ')'.  */

void
function_reader::parse_insn_chain ()
{
  while (1)
    {
      int c = read_skip_spaces ();
      file_location loc = get_current_location ();
      if (c == ')')
	break;
      else if (c == '(')
	{
	  struct md_name directive;
	  read_name (&directive);
	  if (strcmp (directive.string, "block") == 0)
	    parse_block ();
	  else
	    parse_insn (loc, directive.string);
	}
      else
	fatal_at (loc, "expected '(' or ')'");
    }

  create_edges ();
}

/* Parse zero or more child directives (edges and insns) within a
   "block" directive, having already parsed the "(block " heading.
   Consume the trailing ')'.  */

void
function_reader::parse_block ()
{
  /* Parse the index value from the dump.  This will be an integer;
     we don't support "entry" or "exit" here (unlike for edges).  */
  struct md_name name;
  read_name (&name);
  int bb_idx = atoi (name.string);

  /* The term "index" has two meanings for basic blocks in a CFG:
     (a) the "index" field within struct basic_block_def.
     (b) the index of a basic_block within the cfg's x_basic_block_info
     vector, as accessed via BASIC_BLOCK_FOR_FN.

     These can get out-of-sync when basic blocks are optimized away.
     They get back in sync by "compact_blocks".
     We reconstruct cfun->cfg->x_basic_block_info->m_vecdata with NULL
     values in it for any missing basic blocks, so that (a) == (b) for
     all of the blocks we create.  The doubly-linked list of basic
     blocks (next_bb/prev_bb) skips over these "holes".  */

  if (m_highest_bb_idx < bb_idx)
    m_highest_bb_idx = bb_idx;

  size_t new_size = m_highest_bb_idx + 1;
  if (basic_block_info_for_fn (cfun)->length () < new_size)
    vec_safe_grow_cleared (basic_block_info_for_fn (cfun), new_size, true);

  last_basic_block_for_fn (cfun) = new_size;

  /* Create the basic block.

     We can't call create_basic_block and use the regular RTL block-creation
     hooks, since this creates NOTE_INSN_BASIC_BLOCK instances.  We don't
     want to do that; we want to use the notes we were provided with.  */
  basic_block bb = alloc_block ();
  init_rtl_bb_info (bb);
  bb->index = bb_idx;
  bb->flags = BB_NEW | BB_RTL;
  link_block (bb, m_bb_to_insert_after);
  m_bb_to_insert_after = bb;

  n_basic_blocks_for_fn (cfun)++;
  SET_BASIC_BLOCK_FOR_FN (cfun, bb_idx, bb);
  BB_SET_PARTITION (bb, BB_UNPARTITIONED);

  /* Handle insns, edge-from and edge-to directives.  */
  while (1)
    {
      int c = read_skip_spaces ();
      file_location loc = get_current_location ();
      if (c == ')')
	break;
      else if (c == '(')
	{
	  struct md_name directive;
	  read_name (&directive);
	  if (strcmp (directive.string, "edge-from") == 0)
	    parse_edge (bb, true);
	  else if (strcmp (directive.string, "edge-to") == 0)
	    parse_edge (bb, false);
	  else
	    {
	      rtx_insn *insn = parse_insn (loc, directive.string);
	      set_block_for_insn (insn, bb);
	      if (!BB_HEAD (bb))
		BB_HEAD (bb) = insn;
	      BB_END (bb) = insn;
	    }
	}
      else
	fatal_at (loc, "expected '(' or ')'");
    }
}

/* Subroutine of function_reader::parse_edge.
   Parse a basic block index, handling "entry" and "exit".  */

int
function_reader::parse_bb_idx ()
{
  struct md_name name;
  read_name (&name);
  if (strcmp (name.string, "entry") == 0)
    return ENTRY_BLOCK;
  if (strcmp (name.string, "exit") == 0)
    return EXIT_BLOCK;
  return atoi (name.string);
}

/* Subroutine of parse_edge_flags.
   Parse TOK, a token such as "FALLTHRU", converting to the flag value.
   Issue an error if the token is unrecognized.  */

static int
parse_edge_flag_token (const char *tok)
{
#define DEF_EDGE_FLAG(NAME,IDX)		\
  do {						\
    if (strcmp (tok, #NAME) == 0)		\
      return EDGE_##NAME; \
  } while (0);
#include "cfg-flags.def"
#undef DEF_EDGE_FLAG
  error ("unrecognized edge flag: %qs", tok);
  return 0;
}

/* Subroutine of function_reader::parse_edge.
   Parse STR and convert to a flag value (or issue an error).
   The parser uses strtok and hence modifiers STR in-place.  */

static int
parse_edge_flags (char *str)
{
  int result = 0;

  char *tok = strtok (str, "| ");
  while (tok)
    {
      result |= parse_edge_flag_token (tok);
      tok = strtok (NULL, "| ");
    }

  return result;
}

/* Parse an "edge-from" or "edge-to" directive within the "block"
   directive for BLOCK, having already parsed the "(edge" heading.
   Consume the final ")".  Record the edge within m_deferred_edges.
   FROM is true for an "edge-from" directive, false for an "edge-to"
   directive.  */

void
function_reader::parse_edge (basic_block block, bool from)
{
  gcc_assert (block);
  int this_bb_idx = block->index;
  file_location loc = get_current_location ();
  int other_bb_idx = parse_bb_idx ();

  /* "(edge-from 2)" means src = 2, dest = this_bb_idx, whereas
     "(edge-to 3)" means src = this_bb_idx, dest = 3.  */
  int src_idx = from ? other_bb_idx : this_bb_idx;
  int dest_idx = from ? this_bb_idx : other_bb_idx;

  /* Optional "(flags)".  */
  int flags = 0;
  int c = read_skip_spaces ();
  if (c == '(')
    {
      require_word_ws ("flags");
      require_char_ws ('"');
      char *str = read_quoted_string ();
      flags = parse_edge_flags (str);
      require_char_ws (')');
    }
  else
    unread_char (c);

  require_char_ws (')');

  /* This BB already exists, but the other BB might not yet.
     For now, save the edges, and create them at the end of insn-chain
     processing. */
  /* For now, only process the (edge-from) to this BB, and (edge-to)
     that go to the exit block.
     FIXME: we don't yet verify that the edge-from and edge-to directives
     are consistent.  */
  if (from || dest_idx == EXIT_BLOCK)
    m_deferred_edges.safe_push (deferred_edge (loc, src_idx, dest_idx, flags));
}

/* Parse an rtx instruction, having parsed the opening and parenthesis, and
   name NAME, seen at START_LOC, by calling read_rtx_code, calling
   set_first_insn and set_last_insn as appropriate, and
   adding the insn to the insn chain.
   Consume the trailing ')'.  */

rtx_insn *
function_reader::parse_insn (file_location start_loc, const char *name)
{
  rtx x = read_rtx_code (name);
  if (!x)
    fatal_at (start_loc, "expected insn type; got '%s'", name);
  rtx_insn *insn = dyn_cast <rtx_insn *> (x);
  if (!insn)
    fatal_at (start_loc, "expected insn type; got '%s'", name);

  /* Consume the trailing ')'.  */
  require_char_ws (')');

  rtx_insn *last_insn = get_last_insn ();

  /* Add "insn" to the insn chain.  */
  if (last_insn)
    {
      gcc_assert (NEXT_INSN (last_insn) == NULL);
      SET_NEXT_INSN (last_insn) = insn;
    }
  SET_PREV_INSN (insn) = last_insn;

  /* Add it to the sequence.  */
  set_last_insn (insn);
  if (!m_first_insn)
    {
      m_first_insn = insn;
      set_first_insn (insn);
    }

  if (rtx_code_label *label = dyn_cast <rtx_code_label *> (insn))
    maybe_set_max_label_num (label);

  return insn;
}

/* Postprocessing subroutine for parse_insn_chain: all the basic blocks
   should have been created by now; create the edges that were seen.  */

void
function_reader::create_edges ()
{
  int i;
  deferred_edge *de;
  FOR_EACH_VEC_ELT (m_deferred_edges, i, de)
    {
      /* The BBs should already have been created by parse_block.  */
      basic_block src = BASIC_BLOCK_FOR_FN (cfun, de->m_src_bb_idx);
      if (!src)
	fatal_at (de->m_loc, "error: block index %i not found",
		  de->m_src_bb_idx);
      basic_block dst = BASIC_BLOCK_FOR_FN (cfun, de->m_dest_bb_idx);
      if (!dst)
	fatal_at (de->m_loc, "error: block with index %i not found",
		  de->m_dest_bb_idx);
      unchecked_make_edge (src, dst, de->m_flags);
    }
}

/* Parse a "crtl" directive, having already parsed the "(crtl" heading
   at location LOC.
   Consume the final ")".  */

void
function_reader::parse_crtl (file_location loc)
{
  if (m_have_crtl_directive)
    error_at (loc, "more than one 'crtl' directive");
  m_have_crtl_directive = true;

  /* return_rtx.  */
  require_char_ws ('(');
  require_word_ws ("return_rtx");
  crtl->return_rtx = parse_rtx ();
  require_char_ws (')');

  require_char_ws (')');
}

/* Parse operand IDX of X, returning X, or an equivalent rtx
   expression (for consolidating singletons).
   This is an overridden implementation of rtx_reader::read_rtx_operand for
   function_reader, handling various extra data printed by print_rtx,
   and sometimes calling the base class implementation.  */

rtx
function_reader::read_rtx_operand (rtx x, int idx)
{
  RTX_CODE code = GET_CODE (x);
  const char *format_ptr = GET_RTX_FORMAT (code);
  const char format_char = format_ptr[idx];
  struct md_name name;

  /* Override the regular parser for some format codes.  */
  switch (format_char)
    {
    case 'e':
      if (idx == 7 && CALL_P (x))
	{
	  m_in_call_function_usage = true;
	  return rtx_reader::read_rtx_operand (x, idx);
	  m_in_call_function_usage = false;
	}
      else
	return rtx_reader::read_rtx_operand (x, idx);
      break;

    case 'u':
      read_rtx_operand_u (x, idx);
      /* Don't run regular parser for 'u'.  */
      return x;

    case 'i':
    case 'n':
      read_rtx_operand_i_or_n (x, idx, format_char);
      /* Don't run regular parser for these codes.  */
      return x;

    case 'B':
      gcc_assert (is_compact ());
      /* Compact mode doesn't store BBs.  */
      /* Don't run regular parser.  */
      return x;

    case 'r':
      /* Don't run regular parser for 'r'.  */
      return read_rtx_operand_r (x);

    default:
      break;
    }

  /* Call base class implementation.  */
  x = rtx_reader::read_rtx_operand (x, idx);

  /* Handle any additional parsing needed to handle what the dump
     could contain.  */
  switch (format_char)
    {
    case '0':
      x = extra_parsing_for_operand_code_0 (x, idx);
      break;

    case 'w':
      if (!is_compact ())
	{
	  /* Strip away the redundant hex dump of the value.  */
	  require_char_ws ('[');
	  read_name (&name);
	  require_char_ws (']');
	}
      break;

    default:
      break;
    }

  return x;
}

/* Parse operand IDX of X, of code 'u', when reading function dumps.

   The RTL file recorded the ID of an insn (or 0 for NULL); we
   must store this as a pointer, but the insn might not have
   been loaded yet.  Store the ID away for now, via a fixup.  */

void
function_reader::read_rtx_operand_u (rtx x, int idx)
{
  /* In compact mode, the PREV/NEXT insn uids are not dumped, so skip
     the "uu" when reading. */
  if (is_compact () && GET_CODE (x) != LABEL_REF)
    return;

  struct md_name name;
  file_location loc = read_name (&name);
  int insn_id = atoi (name.string);
  if (insn_id)
    add_fixup_insn_uid (loc, x, idx, insn_id);
}

/* Read a name, looking for a match against a string found in array
   STRINGS of size NUM_VALUES.
   Return the index of the matched string, or emit an error.  */

int
function_reader::parse_enum_value (int num_values, const char *const *strings)
{
  struct md_name name;
  read_name (&name);
  for (int i = 0; i < num_values; i++)
    {
      if (strcmp (name.string, strings[i]) == 0)
	return i;
    }
  error ("unrecognized enum value: %qs", name.string);
  return 0;
}

/* Parse operand IDX of X, of code 'i' or 'n' (as specified by FORMAT_CHAR).
   Special-cased handling of these, for reading function dumps.  */

void
function_reader::read_rtx_operand_i_or_n (rtx x, int idx,
					  char format_char)
{
  /* Handle some of the extra information that print_rtx
     can write out for these cases.  */
  /* print_rtx only writes out operand 5 for notes
     for NOTE_KIND values NOTE_INSN_DELETED_LABEL
     and NOTE_INSN_DELETED_DEBUG_LABEL.  */
  if (idx == 5 && NOTE_P (x))
    return;

  if (idx == 4 && INSN_P (x))
    {
      maybe_read_location (as_a <rtx_insn *> (x));
      return;
    }

  /* INSN_CODEs aren't printed in compact mode, so don't attempt to
     parse them.  */
  if (is_compact ()
      && INSN_P (x)
      && &INSN_CODE (x) == &XINT (x, idx))
    {
      INSN_CODE (x) = -1;
      return;
    }

  /* Handle UNSPEC and UNSPEC_VOLATILE's operand 1.  */
#if !defined(GENERATOR_FILE) && NUM_UNSPECV_VALUES > 0
  if (idx == 1
      && GET_CODE (x) == UNSPEC_VOLATILE)
    {
      XINT (x, 1)
	= parse_enum_value (NUM_UNSPECV_VALUES, unspecv_strings);
      return;
    }
#endif
#if !defined(GENERATOR_FILE) && NUM_UNSPEC_VALUES > 0
  if (idx == 1
      && (GET_CODE (x) == UNSPEC
	  || GET_CODE (x) == UNSPEC_VOLATILE))
    {
      XINT (x, 1)
	= parse_enum_value (NUM_UNSPEC_VALUES, unspec_strings);
      return;
    }
#endif

  struct md_name name;
  read_name (&name);
  int value;
  if (format_char == 'n')
    value = parse_note_insn_name (name.string);
  else
    value = atoi (name.string);
  XINT (x, idx) = value;
}

/* Parse the 'r' operand of X, returning X, or an equivalent rtx
   expression (for consolidating singletons).
   Special-cased handling of code 'r' for reading function dumps.  */

rtx
function_reader::read_rtx_operand_r (rtx x)
{
  struct md_name name;
  file_location loc = read_name (&name);
  int regno = lookup_reg_by_dump_name (name.string);
  if (regno == -1)
    fatal_at (loc, "unrecognized register: '%s'", name.string);

  set_regno_raw (x, regno, 1);

  /* Consolidate singletons.  */
  x = consolidate_singletons (x);

  ORIGINAL_REGNO (x) = regno;

  /* Parse extra stuff at end of 'r'.
     We may have zero, one, or two sections marked by square
     brackets.  */
  int ch = read_skip_spaces ();
  bool expect_original_regno = false;
  if (ch == '[')
    {
      file_location loc = get_current_location ();
      char *desc = read_until ("]", true);
      strip_trailing_whitespace (desc);
      const char *desc_start = desc;
      /* If ORIGINAL_REGNO (rtx) != regno, we will have:
	 "orig:%i", ORIGINAL_REGNO (rtx).
	 Consume it, we don't set ORIGINAL_REGNO, since we can
	 get that from the 2nd copy later.  */
      if (startswith (desc, "orig:"))
	{
	  expect_original_regno = true;
	  desc_start += 5;
	  /* Skip to any whitespace following the integer.  */
	  const char *space = strchr (desc_start, ' ');
	  if (space)
	    desc_start = space + 1;
	}
      /* Any remaining text may be the REG_EXPR.  Alternatively we have
	 no REG_ATTRS, and instead we have ORIGINAL_REGNO.  */
      if (ISDIGIT (*desc_start))
	{
	  /* Assume we have ORIGINAL_REGNO.  */
	  ORIGINAL_REGNO (x) = atoi (desc_start);
	}
      else
	{
	  /* Assume we have REG_EXPR.  */
	  add_fixup_expr (loc, x, desc_start);
	}
      free (desc);
    }
  else
    unread_char (ch);
  if (expect_original_regno)
    {
      require_char_ws ('[');
      char *desc = read_until ("]", true);
      ORIGINAL_REGNO (x) = atoi (desc);
      free (desc);
    }

  return x;
}

/* Additional parsing for format code '0' in dumps, handling a variety
   of special-cases in print_rtx, when parsing operand IDX of X.
   Return X, or possibly a reallocated copy of X.  */

rtx
function_reader::extra_parsing_for_operand_code_0 (rtx x, int idx)
{
  RTX_CODE code = GET_CODE (x);
  int c;
  struct md_name name;

  if (idx == 1 && code == SYMBOL_REF)
    {
      /* Possibly wrote " [flags %#x]", SYMBOL_REF_FLAGS (in_rtx).  */
      c = read_skip_spaces ();
      if (c == '[')
	{
	  file_location loc = read_name (&name);
	  if (strcmp (name.string, "flags"))
	    error_at (loc, "was expecting `%s'", "flags");
	  read_name (&name);
	  SYMBOL_REF_FLAGS (x) = strtol (name.string, NULL, 16);

	  /* The standard RTX_CODE_SIZE (SYMBOL_REF) used when allocating
	     x doesn't have space for the block_symbol information, so
	     we must reallocate it if this flag is set.  */
	  if (SYMBOL_REF_HAS_BLOCK_INFO_P (x))
	    {
	      /* Emulate the allocation normally done by
		 varasm.c:create_block_symbol.  */
	      unsigned int size = RTX_HDR_SIZE + sizeof (struct block_symbol);
	      rtx new_x = (rtx) ggc_internal_alloc (size);

	      /* Copy data over from the smaller SYMBOL_REF.  */
	      memcpy (new_x, x, RTX_CODE_SIZE (SYMBOL_REF));
	      x = new_x;

	      /* We can't reconstruct SYMBOL_REF_BLOCK; set it to NULL.  */
	      SYMBOL_REF_BLOCK (x) = NULL;

	      /* Zero the offset.  */
	      SYMBOL_REF_BLOCK_OFFSET (x) = 0;
	    }

	  require_char (']');
	}
      else
	unread_char (c);

      /* If X had a non-NULL SYMBOL_REF_DECL,
	 rtx_writer::print_rtx_operand_code_0 would have dumped it
	 using print_node_brief.
	 Skip the content for now.  */
      c = read_skip_spaces ();
      if (c == '<')
	{
	  while (1)
	    {
	      char ch = read_char ();
	      if (ch == '>')
		break;
	    }
	}
      else
	unread_char (c);
    }
  else if (idx == 3 && code == NOTE)
    {
      /* Note-specific data appears for operand 3, which annoyingly
	 is before the enum specifying which kind of note we have
	 (operand 4).  */
      c = read_skip_spaces ();
      if (c == '[')
	{
	  /* Possibly data for a NOTE_INSN_BASIC_BLOCK, of the form:
	     [bb %d].  */
	  file_location bb_loc = read_name (&name);
	  if (strcmp (name.string, "bb"))
	    error_at (bb_loc, "was expecting `%s'", "bb");
	  read_name (&name);
	  int bb_idx = atoi (name.string);
	  add_fixup_note_insn_basic_block (bb_loc, x, idx,
					   bb_idx);
	  require_char_ws (']');
	}
      else
	unread_char (c);
    }

  return x;
}

/* Implementation of rtx_reader::handle_any_trailing_information.
   Handle the various additional information that print-rtl.c can
   write after the regular fields, when parsing X.  */

void
function_reader::handle_any_trailing_information (rtx x)
{
  struct md_name name;

  switch (GET_CODE (x))
    {
      case MEM:
	{
	  int ch;
	  require_char_ws ('[');
	  read_name (&name);
	  set_mem_alias_set (x, atoi (name.string));
	  /* We have either a MEM_EXPR, or a space.  */
	  if (peek_char () != ' ')
	    {
	      file_location loc = get_current_location ();
	      char *desc = read_until (" +", false);
	      add_fixup_expr (loc, consolidate_singletons (x), desc);
	      free (desc);
	    }
	  else
	    read_char ();

	  /* We may optionally have '+' for MEM_OFFSET_KNOWN_P.  */
	  ch = read_skip_spaces ();
	  if (ch == '+')
	    {
	      read_name (&name);
	      set_mem_offset (x, atoi (name.string));
	    }
	  else
	    unread_char (ch);

	  /* Handle optional " S" for MEM_SIZE.  */
	  ch = read_skip_spaces ();
	  if (ch == 'S')
	    {
	      read_name (&name);
	      set_mem_size (x, atoi (name.string));
	    }
	  else
	    unread_char (ch);

	  /* Handle optional " A" for MEM_ALIGN.  */
	  ch = read_skip_spaces ();
	  if (ch == 'A' && peek_char () != 'S')
	    {
	      read_name (&name);
	      set_mem_align (x, atoi (name.string));
	    }
	  else
	    unread_char (ch);

	  /* Handle optional " AS" for MEM_ADDR_SPACE.  */
	  ch = read_skip_spaces ();
	  if (ch == 'A' && peek_char () == 'S')
	    {
	      read_char ();
	      read_name (&name);
	      set_mem_addr_space (x, atoi (name.string));
	    }
	  else
	    unread_char (ch);

	  require_char (']');
	}
	break;

      case CODE_LABEL:
	/* Assume that LABEL_NUSES was not dumped.  */
	/* TODO: parse LABEL_KIND.  */
	/* For now, skip until closing ')'.  */
	do
	  {
	    char ch = read_char ();
	    if (ch == ')')
	      {
		unread_char (ch);
		break;
	      }
	  }
	while (1);
	break;

      default:
	break;
    }
}

/* Parse a tree dump for a MEM_EXPR in DESC and turn it back into a tree.
   We handle "<retval>" and param names within cfun, but for anything else
   we "cheat" by building a global VAR_DECL of type "int" with that name
   (returning the same global for a name if we see the same name more
   than once).  */

tree
function_reader::parse_mem_expr (const char *desc)
{
  tree fndecl = cfun->decl;

  if (strcmp (desc, "<retval>") == 0)
    return DECL_RESULT (fndecl);

  tree param = find_param_by_name (fndecl, desc);
  if (param)
    return param;

  /* Search within decls we already created.
     FIXME: use a hash rather than linear search.  */
  int i;
  tree t;
  FOR_EACH_VEC_ELT (m_fake_scope, i, t)
    if (id_equal (DECL_NAME (t), desc))
      return t;

  /* Not found?  Create it.
     This allows mimicking of real data but avoids having to specify
     e.g. names of locals, params etc.
     Though this way we don't know if we have a PARM_DECL vs a VAR_DECL,
     and we don't know the types.  Fake it by making everything be
     a VAR_DECL of "int" type.  */
  t = build_decl (UNKNOWN_LOCATION, VAR_DECL,
		  get_identifier (desc),
		  integer_type_node);
  m_fake_scope.safe_push (t);
  return t;
}

/* Record that at LOC we saw an insn uid INSN_UID for the operand with index
   OPERAND_IDX within INSN, so that the pointer value can be fixed up in
   later post-processing.  */

void
function_reader::add_fixup_insn_uid (file_location loc, rtx insn, int operand_idx,
				     int insn_uid)
{
  m_fixups.safe_push (new fixup_insn_uid (loc, insn, operand_idx, insn_uid));
}

/* Record that at LOC we saw an basic block index BB_IDX for the operand with index
   OPERAND_IDX within INSN, so that the pointer value can be fixed up in
   later post-processing.  */

void
function_reader::add_fixup_note_insn_basic_block (file_location loc, rtx insn,
						  int operand_idx, int bb_idx)
{
  m_fixups.safe_push (new fixup_note_insn_basic_block (loc, insn, operand_idx,
						       bb_idx));
}

/* Placeholder hook for recording source location information seen in a dump.
   This is empty for now.  */

void
function_reader::add_fixup_source_location (file_location, rtx_insn *,
					    const char *, int, int)
{
}

/* Record that at LOC we saw textual description DESC of the MEM_EXPR or REG_EXPR
   of INSN, so that the fields can be fixed up in later post-processing.  */

void
function_reader::add_fixup_expr (file_location loc, rtx insn,
				 const char *desc)
{
  gcc_assert (desc);
  /* Fail early if the RTL reader erroneously hands us an int.  */
  gcc_assert (!ISDIGIT (desc[0]));

  m_fixups.safe_push (new fixup_expr (loc, insn, desc));
}

/* Helper function for consolidate_reg.  Return the global rtx for
   the register with regno REGNO.  */

static rtx
lookup_global_register (int regno)
{
  /* We can't use a switch here, as some of the REGNUMs might not be constants
     for some targets.  */
  if (regno == STACK_POINTER_REGNUM)
      return stack_pointer_rtx;
  else if (regno ==  FRAME_POINTER_REGNUM)
    return frame_pointer_rtx;
  else if (regno == HARD_FRAME_POINTER_REGNUM)
    return hard_frame_pointer_rtx;
  else if (regno == ARG_POINTER_REGNUM)
    return arg_pointer_rtx;
  else if (regno == VIRTUAL_INCOMING_ARGS_REGNUM)
    return virtual_incoming_args_rtx;
  else if (regno == VIRTUAL_STACK_VARS_REGNUM)
    return virtual_stack_vars_rtx;
  else if (regno == VIRTUAL_STACK_DYNAMIC_REGNUM)
    return virtual_stack_dynamic_rtx;
  else if (regno == VIRTUAL_OUTGOING_ARGS_REGNUM)
    return virtual_outgoing_args_rtx;
  else if (regno == VIRTUAL_CFA_REGNUM)
    return virtual_cfa_rtx;
  else if (regno == VIRTUAL_PREFERRED_STACK_BOUNDARY_REGNUM)
    return virtual_preferred_stack_boundary_rtx;
#ifdef return_ADDRESS_POINTER_REGNUM
  else if (regno == RETURN_ADDRESS_POINTER_REGNUM)
    return return_address_pointer_rtx;
#endif

  return NULL;
}

/* Ensure that the backend can cope with a REG with regno REGNO.
   Normally REG instances are created by gen_reg_rtx which updates
   regno_reg_rtx, growing it as necessary.
   The REG instances created from the dumpfile weren't created this
   way, so we need to manually update regno_reg_rtx.  */

static void
ensure_regno (int regno)
{
  if (reg_rtx_no < regno + 1)
    reg_rtx_no = regno + 1;

  crtl->emit.ensure_regno_capacity ();
  gcc_assert (regno < crtl->emit.regno_pointer_align_length);
}

/* Helper function for consolidate_singletons, for handling REG instances.
   Given REG instance X of some regno, return the singleton rtx for that
   regno, if it exists, or X.  */

static rtx
consolidate_reg (rtx x)
{
  gcc_assert (GET_CODE (x) == REG);

  unsigned int regno = REGNO (x);

  ensure_regno (regno);

  /* Some register numbers have their rtx created in init_emit_regs
     e.g. stack_pointer_rtx for STACK_POINTER_REGNUM.
     Consolidate on this.  */
  rtx global_reg = lookup_global_register (regno);
  if (global_reg)
    return global_reg;

  /* Populate regno_reg_rtx if necessary.  */
  if (regno_reg_rtx[regno] == NULL)
    regno_reg_rtx[regno] = x;
  /* Use it.  */
  gcc_assert (GET_CODE (regno_reg_rtx[regno]) == REG);
  gcc_assert (REGNO (regno_reg_rtx[regno]) == regno);
  if (GET_MODE (x) == GET_MODE (regno_reg_rtx[regno]))
    return regno_reg_rtx[regno];

  return x;
}

/* When reading RTL function dumps, we must consolidate some
   rtx so that we use singletons where singletons are expected
   (e.g. we don't want multiple "(const_int 0 [0])" rtx, since
   these are tested via pointer equality against const0_rtx.

   Return the equivalent singleton rtx for X, if any, otherwise X.  */

rtx
function_reader::consolidate_singletons (rtx x)
{
  if (!x)
    return x;

  switch (GET_CODE (x))
    {
    case PC: return pc_rtx;
    case RETURN: return ret_rtx;
    case SIMPLE_RETURN: return simple_return_rtx;

    case REG:
      return consolidate_reg (x);

    case CONST_INT:
      return gen_rtx_CONST_INT (GET_MODE (x), INTVAL (x));

    default:
      break;
    }

  return x;
}

/* Parse an rtx directive, including both the opening/closing parentheses,
   and the name.  */

rtx
function_reader::parse_rtx ()
{
  require_char_ws ('(');
  struct md_name directive;
  read_name (&directive);
  rtx result
    = consolidate_singletons (read_rtx_code (directive.string));
  require_char_ws (')');

  return result;
}

/* Implementation of rtx_reader::postprocess for reading function dumps.
   Return the equivalent singleton rtx for X, if any, otherwise X.  */

rtx
function_reader::postprocess (rtx x)
{
  return consolidate_singletons (x);
}

/* Implementation of rtx_reader::finalize_string for reading function dumps.
   Make a GC-managed copy of STRINGBUF.  */

const char *
function_reader::finalize_string (char *stringbuf)
{
  return ggc_strdup (stringbuf);
}

/* Attempt to parse optional location information for insn INSN, as
   potentially written out by rtx_writer::print_rtx_operand_code_i.
   We look for a quoted string followed by a colon.  */

void
function_reader::maybe_read_location (rtx_insn *insn)
{
  file_location loc = get_current_location ();

  /* Attempt to parse a quoted string.  */
  int ch = read_skip_spaces ();
  if (ch == '"')
    {
      char *filename = read_quoted_string ();
      require_char (':');
      struct md_name line_num;
      read_name (&line_num);

      int column = 0;
      int ch = read_char ();
      if (ch == ':')
	{
	  struct md_name column_num;
	  read_name (&column_num);
	  column = atoi (column_num.string);
	}
      else
	unread_char (ch);
      add_fixup_source_location (loc, insn, filename,
				 atoi (line_num.string),
				 column);
    }
  else
    unread_char (ch);
}

/* Postprocessing subroutine of function_reader::parse_function.
   Populate m_insns_by_uid.  */

void
function_reader::handle_insn_uids ()
{
  /* Locate the currently assigned INSN_UID values, storing
     them in m_insns_by_uid.  */
  int max_uid = 0;
  for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (m_insns_by_uid.get (INSN_UID (insn)))
	error ("duplicate insn UID: %i", INSN_UID (insn));
      m_insns_by_uid.put (INSN_UID (insn), insn);
      if (INSN_UID (insn) > max_uid)
	max_uid = INSN_UID (insn);
    }

  /* Ensure x_cur_insn_uid is 1 more than the biggest insn UID seen.
     This is normally updated by the various make_*insn_raw functions.  */
  crtl->emit.x_cur_insn_uid = max_uid + 1;
}

/* Apply all of the recorded fixups.  */

void
function_reader::apply_fixups ()
{
  int i;
  fixup *f;
  FOR_EACH_VEC_ELT (m_fixups, i, f)
    f->apply (this);
}

/* Given a UID value, try to locate a pointer to the corresponding
   rtx_insn *, or NULL if it can't be found.  */

rtx_insn **
function_reader::get_insn_by_uid (int uid)
{
  return m_insns_by_uid.get (uid);
}

/* Run the RTL dump parser, parsing a dump located at PATH.
   Return true iff the file was successfully parsed.  */

bool
read_rtl_function_body (const char *path)
{
  initialize_rtl ();
  crtl->abi = &default_function_abi;
  init_emit ();
  init_varasm_status ();

  function_reader reader;
  if (!reader.read_file (path))
    return false;

  return true;
}

/* Run the RTL dump parser on the range of lines between START_LOC and
   END_LOC (including those lines).  */

bool
read_rtl_function_body_from_file_range (location_t start_loc,
					location_t end_loc)
{
  expanded_location exploc_start = expand_location (start_loc);
  expanded_location exploc_end = expand_location (end_loc);

  if (exploc_start.file != exploc_end.file)
    {
      error_at (end_loc, "start/end of RTL fragment are in different files");
      return false;
    }
  if (exploc_start.line >= exploc_end.line)
    {
      error_at (end_loc,
		"start of RTL fragment must be on an earlier line than end");
      return false;
    }

  initialize_rtl ();
  crtl->abi = &fndecl_abi (cfun->decl).base_abi ();
  init_emit ();
  init_varasm_status ();

  function_reader reader;
  if (!reader.read_file_fragment (exploc_start.file, exploc_start.line,
				  exploc_end.line - 1))
    return false;

  return true;
}

#if CHECKING_P

namespace selftest {

/* Verify that parse_edge_flags works.  */

static void
test_edge_flags ()
{
  /* parse_edge_flags modifies its input (due to strtok), so we must make
     a copy of the literals.  */
#define ASSERT_PARSE_EDGE_FLAGS(EXPECTED, STR) \
  do { \
    char *str = xstrdup (STR); \
    ASSERT_EQ (EXPECTED, parse_edge_flags (str)); \
    free (str); \
  } while (0)

  ASSERT_PARSE_EDGE_FLAGS (0, "");
  ASSERT_PARSE_EDGE_FLAGS (EDGE_FALLTHRU, "FALLTHRU");
  ASSERT_PARSE_EDGE_FLAGS (EDGE_ABNORMAL_CALL, "ABNORMAL_CALL");
  ASSERT_PARSE_EDGE_FLAGS (EDGE_ABNORMAL | EDGE_ABNORMAL_CALL,
			   "ABNORMAL | ABNORMAL_CALL");

#undef  ASSERT_PARSE_EDGE_FLAGS
}

/* Verify that lookup_reg_by_dump_name works.  */

static void
test_parsing_regnos ()
{
  ASSERT_EQ (-1, lookup_reg_by_dump_name ("this is not a register"));

  /* Verify lookup of virtual registers.  */
  ASSERT_EQ (VIRTUAL_INCOMING_ARGS_REGNUM,
    lookup_reg_by_dump_name ("virtual-incoming-args"));
  ASSERT_EQ (VIRTUAL_STACK_VARS_REGNUM,
    lookup_reg_by_dump_name ("virtual-stack-vars"));
  ASSERT_EQ (VIRTUAL_STACK_DYNAMIC_REGNUM,
    lookup_reg_by_dump_name ("virtual-stack-dynamic"));
  ASSERT_EQ (VIRTUAL_OUTGOING_ARGS_REGNUM,
    lookup_reg_by_dump_name ("virtual-outgoing-args"));
  ASSERT_EQ (VIRTUAL_CFA_REGNUM,
    lookup_reg_by_dump_name ("virtual-cfa"));
  ASSERT_EQ (VIRTUAL_PREFERRED_STACK_BOUNDARY_REGNUM,
    lookup_reg_by_dump_name ("virtual-preferred-stack-boundary"));

  /* Verify lookup of non-virtual pseudos.  */
  ASSERT_EQ (LAST_VIRTUAL_REGISTER + 1, lookup_reg_by_dump_name ("<0>"));
  ASSERT_EQ (LAST_VIRTUAL_REGISTER + 2, lookup_reg_by_dump_name ("<1>"));
}

/* Verify that edge E is as expected, with the src and dest basic blocks
   having indices EXPECTED_SRC_IDX and EXPECTED_DEST_IDX respectively, and
   the edge having flags equal to EXPECTED_FLAGS.
   Use LOC as the effective location when reporting failures.  */

static void
assert_edge_at (const location &loc, edge e, int expected_src_idx,
		int expected_dest_idx, int expected_flags)
{
  ASSERT_EQ_AT (loc, expected_src_idx, e->src->index);
  ASSERT_EQ_AT (loc, expected_dest_idx, e->dest->index);
  ASSERT_EQ_AT (loc, expected_flags, e->flags);
}

/* Verify that edge EDGE is as expected, with the src and dest basic blocks
   having indices EXPECTED_SRC_IDX and EXPECTED_DEST_IDX respectively, and
   the edge having flags equal to EXPECTED_FLAGS.  */

#define ASSERT_EDGE(EDGE, EXPECTED_SRC_IDX, EXPECTED_DEST_IDX,		\
		    EXPECTED_FLAGS)					\
  assert_edge_at (SELFTEST_LOCATION, EDGE, EXPECTED_SRC_IDX, \
		  EXPECTED_DEST_IDX, EXPECTED_FLAGS)

/* Verify that we can load RTL dumps.  */

static void
test_loading_dump_fragment_1 ()
{
  // TODO: filter on target?
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("asr_div1.rtl"));

  /* Verify that the insns were loaded correctly.  */
  rtx_insn *insn_1 = get_insns ();
  ASSERT_TRUE (insn_1);
  ASSERT_EQ (1, INSN_UID (insn_1));
  ASSERT_EQ (INSN, GET_CODE (insn_1));
  ASSERT_EQ (SET, GET_CODE (PATTERN (insn_1)));
  ASSERT_EQ (NULL, PREV_INSN (insn_1));

  rtx_insn *insn_2 = NEXT_INSN (insn_1);
  ASSERT_TRUE (insn_2);
  ASSERT_EQ (2, INSN_UID (insn_2));
  ASSERT_EQ (INSN, GET_CODE (insn_2));
  ASSERT_EQ (insn_1, PREV_INSN (insn_2));
  ASSERT_EQ (NULL, NEXT_INSN (insn_2));

  /* Verify that registers were loaded correctly.  */
  rtx insn_1_dest = SET_DEST (PATTERN (insn_1));
  ASSERT_EQ (REG, GET_CODE (insn_1_dest));
  ASSERT_EQ ((LAST_VIRTUAL_REGISTER + 1) + 2, REGNO (insn_1_dest));
  rtx insn_1_src = SET_SRC (PATTERN (insn_1));
  ASSERT_EQ (LSHIFTRT, GET_CODE (insn_1_src));
  rtx reg = XEXP (insn_1_src, 0);
  ASSERT_EQ (REG, GET_CODE (reg));
  ASSERT_EQ (LAST_VIRTUAL_REGISTER + 1, REGNO (reg));

  /* Verify that get_insn_by_uid works.  */
  ASSERT_EQ (insn_1, get_insn_by_uid (1));
  ASSERT_EQ (insn_2, get_insn_by_uid (2));

  /* Verify that basic blocks were created.  */
  ASSERT_EQ (2, BLOCK_FOR_INSN (insn_1)->index);
  ASSERT_EQ (2, BLOCK_FOR_INSN (insn_2)->index);

  /* Verify that the CFG was recreated.  */
  ASSERT_TRUE (cfun);
  verify_three_block_rtl_cfg (cfun);
  basic_block bb2 = BASIC_BLOCK_FOR_FN (cfun, 2);
  ASSERT_TRUE (bb2 != NULL);
  ASSERT_EQ (BB_RTL, bb2->flags & BB_RTL);
  ASSERT_EQ (2, bb2->index);
  ASSERT_EQ (insn_1, BB_HEAD (bb2));
  ASSERT_EQ (insn_2, BB_END (bb2));
}

/* Verify loading another RTL dump.  */

static void
test_loading_dump_fragment_2 ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("simple-cse.rtl"));

  rtx_insn *insn_1 = get_insn_by_uid (1);
  rtx_insn *insn_2 = get_insn_by_uid (2);
  rtx_insn *insn_3 = get_insn_by_uid (3);

  rtx set1 = single_set (insn_1);
  ASSERT_NE (NULL, set1);
  rtx set2 = single_set (insn_2);
  ASSERT_NE (NULL, set2);
  rtx set3 = single_set (insn_3);
  ASSERT_NE (NULL, set3);

  rtx src1 = SET_SRC (set1);
  ASSERT_EQ (PLUS, GET_CODE (src1));

  rtx src2 = SET_SRC (set2);
  ASSERT_EQ (PLUS, GET_CODE (src2));

  /* Both src1 and src2 refer to "(reg:SI %0)".
     Verify that we have pointer equality.  */
  rtx lhs1 = XEXP (src1, 0);
  rtx lhs2 = XEXP (src2, 0);
  ASSERT_EQ (lhs1, lhs2);

  /* Verify that the CFG was recreated. */
  ASSERT_TRUE (cfun);
  verify_three_block_rtl_cfg (cfun);
}

/* Verify that CODE_LABEL insns are loaded correctly.  */

static void
test_loading_labels ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("example-labels.rtl"));

  rtx_insn *insn_100 = get_insn_by_uid (100);
  ASSERT_EQ (CODE_LABEL, GET_CODE (insn_100));
  ASSERT_EQ (100, INSN_UID (insn_100));
  ASSERT_EQ (NULL, LABEL_NAME (insn_100));
  ASSERT_EQ (0, LABEL_NUSES (insn_100));
  ASSERT_EQ (30, CODE_LABEL_NUMBER (insn_100));

  rtx_insn *insn_200 = get_insn_by_uid (200);
  ASSERT_EQ (CODE_LABEL, GET_CODE (insn_200));
  ASSERT_EQ (200, INSN_UID (insn_200));
  ASSERT_STREQ ("some_label_name", LABEL_NAME (insn_200));
  ASSERT_EQ (0, LABEL_NUSES (insn_200));
  ASSERT_EQ (40, CODE_LABEL_NUMBER (insn_200));

  /* Ensure that the presence of CODE_LABEL_NUMBER == 40
     means that the next label num to be handed out will be 41.  */
  ASSERT_EQ (41, max_label_num ());

  /* Ensure that label names read from a dump are GC-managed
     and are found through the insn.  */
  forcibly_ggc_collect ();
  ASSERT_TRUE (ggc_marked_p (insn_200));
  ASSERT_TRUE (ggc_marked_p (LABEL_NAME (insn_200)));
}

/* Verify that the loader copes with an insn with a mode.  */

static void
test_loading_insn_with_mode ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("insn-with-mode.rtl"));
  rtx_insn *insn = get_insns ();
  ASSERT_EQ (INSN, GET_CODE (insn));

  /* Verify that the "TI" mode was set from "insn:TI".  */
  ASSERT_EQ (TImode, GET_MODE (insn));
}

/* Verify that the loader copes with a jump_insn to a label_ref.  */

static void
test_loading_jump_to_label_ref ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("jump-to-label-ref.rtl"));

  rtx_insn *jump_insn = get_insn_by_uid (1);
  ASSERT_EQ (JUMP_INSN, GET_CODE (jump_insn));

  rtx_insn *barrier = get_insn_by_uid (2);
  ASSERT_EQ (BARRIER, GET_CODE (barrier));

  rtx_insn *code_label = get_insn_by_uid (100);
  ASSERT_EQ (CODE_LABEL, GET_CODE (code_label));

  /* Verify the jump_insn. */
  ASSERT_EQ (4, BLOCK_FOR_INSN (jump_insn)->index);
  ASSERT_EQ (SET, GET_CODE (PATTERN (jump_insn)));
  /* Ensure that the "(pc)" is using the global singleton.  */
  ASSERT_RTX_PTR_EQ (pc_rtx, SET_DEST (PATTERN (jump_insn)));
  rtx label_ref = SET_SRC (PATTERN (jump_insn));
  ASSERT_EQ (LABEL_REF, GET_CODE (label_ref));
  ASSERT_EQ (code_label, label_ref_label (label_ref));
  ASSERT_EQ (code_label, JUMP_LABEL (jump_insn));

  /* Verify the code_label. */
  ASSERT_EQ (5, BLOCK_FOR_INSN (code_label)->index);
  ASSERT_EQ (NULL, LABEL_NAME (code_label));
  ASSERT_EQ (1, LABEL_NUSES (code_label));

  /* Verify the generated CFG.  */

  /* Locate blocks.  */
  basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  ASSERT_TRUE (entry != NULL);
  ASSERT_EQ (ENTRY_BLOCK, entry->index);

  basic_block exit = EXIT_BLOCK_PTR_FOR_FN (cfun);
  ASSERT_TRUE (exit != NULL);
  ASSERT_EQ (EXIT_BLOCK, exit->index);

  basic_block bb4 = (*cfun->cfg->x_basic_block_info)[4];
  basic_block bb5 = (*cfun->cfg->x_basic_block_info)[5];
  ASSERT_EQ (4, bb4->index);
  ASSERT_EQ (5, bb5->index);

  /* Entry block.  */
  ASSERT_EQ (NULL, entry->preds);
  ASSERT_EQ (1, entry->succs->length ());
  ASSERT_EDGE ((*entry->succs)[0], 0, 4, EDGE_FALLTHRU);

  /* bb4.  */
  ASSERT_EQ (1, bb4->preds->length ());
  ASSERT_EDGE ((*bb4->preds)[0], 0, 4, EDGE_FALLTHRU);
  ASSERT_EQ (1, bb4->succs->length ());
  ASSERT_EDGE ((*bb4->succs)[0], 4, 5, 0x0);

  /* bb5.  */
  ASSERT_EQ (1, bb5->preds->length ());
  ASSERT_EDGE ((*bb5->preds)[0], 4, 5, 0x0);
  ASSERT_EQ (1, bb5->succs->length ());
  ASSERT_EDGE ((*bb5->succs)[0], 5, 1, EDGE_FALLTHRU);

  /* Exit block.  */
  ASSERT_EQ (1, exit->preds->length ());
  ASSERT_EDGE ((*exit->preds)[0], 5, 1, EDGE_FALLTHRU);
  ASSERT_EQ (NULL, exit->succs);
}

/* Verify that the loader copes with a jump_insn to a label_ref
   marked "return".  */

static void
test_loading_jump_to_return ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("jump-to-return.rtl"));

  rtx_insn *jump_insn = get_insn_by_uid (1);
  ASSERT_EQ (JUMP_INSN, GET_CODE (jump_insn));
  ASSERT_RTX_PTR_EQ (ret_rtx, JUMP_LABEL (jump_insn));
}

/* Verify that the loader copes with a jump_insn to a label_ref
   marked "simple_return".  */

static void
test_loading_jump_to_simple_return ()
{
  rtl_dump_test t (SELFTEST_LOCATION,
		   locate_file ("jump-to-simple-return.rtl"));

  rtx_insn *jump_insn = get_insn_by_uid (1);
  ASSERT_EQ (JUMP_INSN, GET_CODE (jump_insn));
  ASSERT_RTX_PTR_EQ (simple_return_rtx, JUMP_LABEL (jump_insn));
}

/* Verify that the loader copes with a NOTE_INSN_BASIC_BLOCK.  */

static void
test_loading_note_insn_basic_block ()
{
  rtl_dump_test t (SELFTEST_LOCATION,
		   locate_file ("note_insn_basic_block.rtl"));

  rtx_insn *note = get_insn_by_uid (1);
  ASSERT_EQ (NOTE, GET_CODE (note));
  ASSERT_EQ (2, BLOCK_FOR_INSN (note)->index);

  ASSERT_EQ (NOTE_INSN_BASIC_BLOCK, NOTE_KIND (note));
  ASSERT_EQ (2, NOTE_BASIC_BLOCK (note)->index);
  ASSERT_EQ (BASIC_BLOCK_FOR_FN (cfun, 2), NOTE_BASIC_BLOCK (note));
}

/* Verify that the loader copes with a NOTE_INSN_DELETED.  */

static void
test_loading_note_insn_deleted ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("note-insn-deleted.rtl"));

  rtx_insn *note = get_insn_by_uid (1);
  ASSERT_EQ (NOTE, GET_CODE (note));
  ASSERT_EQ (NOTE_INSN_DELETED, NOTE_KIND (note));
}

/* Verify that the const_int values are consolidated, since
   pointer equality corresponds to value equality.
   TODO: do this for all in CASE_CONST_UNIQUE.  */

static void
test_loading_const_int ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("const-int.rtl"));

  /* Verify that const_int values below MAX_SAVED_CONST_INT use
     the global values.  */
  ASSERT_EQ (const0_rtx, SET_SRC (PATTERN (get_insn_by_uid (1))));
  ASSERT_EQ (const1_rtx, SET_SRC (PATTERN (get_insn_by_uid (2))));
  ASSERT_EQ (constm1_rtx, SET_SRC (PATTERN (get_insn_by_uid (3))));

  /* Verify that other const_int values are consolidated. */
  rtx int256 = gen_rtx_CONST_INT (SImode, 256);
  ASSERT_EQ (int256, SET_SRC (PATTERN (get_insn_by_uid (4))));
}

/* Verify that the loader copes with a SYMBOL_REF.  */

static void
test_loading_symbol_ref ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("symbol-ref.rtl"));

  rtx_insn *insn = get_insns ();

  rtx high = SET_SRC (PATTERN (insn));
  ASSERT_EQ (HIGH, GET_CODE (high));

  rtx symbol_ref = XEXP (high, 0);
  ASSERT_EQ (SYMBOL_REF, GET_CODE (symbol_ref));

  /* Verify that "[flags 0xc0]" was parsed.  */
  ASSERT_EQ (0xc0, SYMBOL_REF_FLAGS (symbol_ref));
  /* TODO: we don't yet load SYMBOL_REF_DECL.  */
}

/* Verify that the loader can rebuild a CFG.  */

static void
test_loading_cfg ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("cfg-test.rtl"));

  ASSERT_STREQ ("cfg_test", IDENTIFIER_POINTER (DECL_NAME (cfun->decl)));

  ASSERT_TRUE (cfun);

  ASSERT_TRUE (cfun->cfg != NULL);
  ASSERT_EQ (6, n_basic_blocks_for_fn (cfun));
  ASSERT_EQ (6, n_edges_for_fn (cfun));

  /* The "fake" basic blocks.  */
  basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  ASSERT_TRUE (entry != NULL);
  ASSERT_EQ (ENTRY_BLOCK, entry->index);

  basic_block exit = EXIT_BLOCK_PTR_FOR_FN (cfun);
  ASSERT_TRUE (exit != NULL);
  ASSERT_EQ (EXIT_BLOCK, exit->index);

  /* The "real" basic blocks.  */
  basic_block bb2 = (*cfun->cfg->x_basic_block_info)[2];
  basic_block bb3 = (*cfun->cfg->x_basic_block_info)[3];
  basic_block bb4 = (*cfun->cfg->x_basic_block_info)[4];
  basic_block bb5 = (*cfun->cfg->x_basic_block_info)[5];

  ASSERT_EQ (2, bb2->index);
  ASSERT_EQ (3, bb3->index);
  ASSERT_EQ (4, bb4->index);
  ASSERT_EQ (5, bb5->index);

  /* Verify connectivity.  */

  /* Entry block.  */
  ASSERT_EQ (NULL, entry->preds);
  ASSERT_EQ (1, entry->succs->length ());
  ASSERT_EDGE ((*entry->succs)[0], 0, 2, EDGE_FALLTHRU);

  /* bb2.  */
  ASSERT_EQ (1, bb2->preds->length ());
  ASSERT_EDGE ((*bb2->preds)[0], 0, 2, EDGE_FALLTHRU);
  ASSERT_EQ (2, bb2->succs->length ());
  ASSERT_EDGE ((*bb2->succs)[0], 2, 3, EDGE_TRUE_VALUE);
  ASSERT_EDGE ((*bb2->succs)[1], 2, 4, EDGE_FALSE_VALUE);

  /* bb3.  */
  ASSERT_EQ (1, bb3->preds->length ());
  ASSERT_EDGE ((*bb3->preds)[0], 2, 3, EDGE_TRUE_VALUE);
  ASSERT_EQ (1, bb3->succs->length ());
  ASSERT_EDGE ((*bb3->succs)[0], 3, 5, EDGE_FALLTHRU);

  /* bb4.  */
  ASSERT_EQ (1, bb4->preds->length ());
  ASSERT_EDGE ((*bb4->preds)[0], 2, 4, EDGE_FALSE_VALUE);
  ASSERT_EQ (1, bb4->succs->length ());
  ASSERT_EDGE ((*bb4->succs)[0], 4, 5, EDGE_FALLTHRU);

  /* bb5.  */
  ASSERT_EQ (2, bb5->preds->length ());
  ASSERT_EDGE ((*bb5->preds)[0], 3, 5, EDGE_FALLTHRU);
  ASSERT_EDGE ((*bb5->preds)[1], 4, 5, EDGE_FALLTHRU);
  ASSERT_EQ (1, bb5->succs->length ());
  ASSERT_EDGE ((*bb5->succs)[0], 5, 1, EDGE_FALLTHRU);

  /* Exit block.  */
  ASSERT_EQ (1, exit->preds->length ());
  ASSERT_EDGE ((*exit->preds)[0], 5, 1, EDGE_FALLTHRU);
  ASSERT_EQ (NULL, exit->succs);
}

/* Verify that the loader copes with sparse block indices.
   This testcase loads a file with a "(block 42)".  */

static void
test_loading_bb_index ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("bb-index.rtl"));

  ASSERT_STREQ ("test_bb_index", IDENTIFIER_POINTER (DECL_NAME (cfun->decl)));

  ASSERT_TRUE (cfun);

  ASSERT_TRUE (cfun->cfg != NULL);
  ASSERT_EQ (3, n_basic_blocks_for_fn (cfun));
  ASSERT_EQ (43, basic_block_info_for_fn (cfun)->length ());
  ASSERT_EQ (2, n_edges_for_fn (cfun));

  ASSERT_EQ (NULL, (*cfun->cfg->x_basic_block_info)[41]);
  basic_block bb42 = (*cfun->cfg->x_basic_block_info)[42];
  ASSERT_NE (NULL, bb42);
  ASSERT_EQ (42, bb42->index);
}

/* Verify that function_reader::handle_any_trailing_information correctly
   parses all the possible items emitted for a MEM.  */

static void
test_loading_mem ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("mem.rtl"));

  ASSERT_STREQ ("test_mem", IDENTIFIER_POINTER (DECL_NAME (cfun->decl)));
  ASSERT_TRUE (cfun);

  /* Verify parsing of "[42 i+17 S8 A128 AS5]".  */
  rtx_insn *insn_1 = get_insn_by_uid (1);
  rtx set1 = single_set (insn_1);
  rtx mem1 = SET_DEST (set1);
  ASSERT_EQ (42, MEM_ALIAS_SET (mem1));
  /* "+17".  */
  ASSERT_TRUE (MEM_OFFSET_KNOWN_P (mem1));
  ASSERT_KNOWN_EQ (17, MEM_OFFSET (mem1));
  /* "S8".  */
  ASSERT_KNOWN_EQ (8, MEM_SIZE (mem1));
  /* "A128.  */
  ASSERT_EQ (128, MEM_ALIGN (mem1));
  /* "AS5.  */
  ASSERT_EQ (5, MEM_ADDR_SPACE (mem1));

  /* Verify parsing of "43 i+18 S9 AS6"
     (an address space without an alignment).  */
  rtx_insn *insn_2 = get_insn_by_uid (2);
  rtx set2 = single_set (insn_2);
  rtx mem2 = SET_DEST (set2);
  ASSERT_EQ (43, MEM_ALIAS_SET (mem2));
  /* "+18".  */
  ASSERT_TRUE (MEM_OFFSET_KNOWN_P (mem2));
  ASSERT_KNOWN_EQ (18, MEM_OFFSET (mem2));
  /* "S9".  */
  ASSERT_KNOWN_EQ (9, MEM_SIZE (mem2));
  /* "AS6.  */
  ASSERT_EQ (6, MEM_ADDR_SPACE (mem2));
}

/* Verify that "repeated xN" is read correctly.  */

static void
test_loading_repeat ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("repeat.rtl"));

  rtx_insn *insn_1 = get_insn_by_uid (1);
  ASSERT_EQ (PARALLEL, GET_CODE (PATTERN (insn_1)));
  ASSERT_EQ (64, XVECLEN (PATTERN (insn_1), 0));
  for (int i = 0; i < 64; i++)
    ASSERT_EQ (const0_rtx, XVECEXP (PATTERN (insn_1), 0, i));
}

/* Run all of the selftests within this file.  */

void
read_rtl_function_c_tests ()
{
  test_edge_flags ();
  test_parsing_regnos ();
  test_loading_dump_fragment_1 ();
  test_loading_dump_fragment_2 ();
  test_loading_labels ();
  test_loading_insn_with_mode ();
  test_loading_jump_to_label_ref ();
  test_loading_jump_to_return ();
  test_loading_jump_to_simple_return ();
  test_loading_note_insn_basic_block ();
  test_loading_note_insn_deleted ();
  test_loading_const_int ();
  test_loading_symbol_ref ();
  test_loading_cfg ();
  test_loading_bb_index ();
  test_loading_mem ();
  test_loading_repeat ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
