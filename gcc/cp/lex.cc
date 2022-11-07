/* Separate lexical analyzer for GNU C++.
   Copyright (C) 1987-2022 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


/* This file is the lexical analyzer for GNU C++.  */

#include "config.h"
/* For use with name_hint.  */
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "c-family/c-pragma.h"
#include "c-family/c-objc.h"
#include "gcc-rich-location.h"
#include "cp-name-hint.h"
#include "langhooks.h"

static int interface_strcmp (const char *);
static void init_cp_pragma (void);

static tree parse_strconst_pragma (const char *, int);
static void handle_pragma_vtable (cpp_reader *);
static void handle_pragma_unit (cpp_reader *);
static void handle_pragma_interface (cpp_reader *);
static void handle_pragma_implementation (cpp_reader *);

static void init_operators (void);
static void copy_lang_type (tree);

/* A constraint that can be tested at compile time.  */
#define CONSTRAINT(name, expr) extern int constraint_##name [(expr) ? 1 : -1]

/* Functions and data structures for #pragma interface.

   `#pragma implementation' means that the main file being compiled
   is considered to implement (provide) the classes that appear in
   its main body.  I.e., if this is file "foo.cc", and class `bar'
   is defined in "foo.cc", then we say that "foo.cc implements bar".

   All main input files "implement" themselves automagically.

   `#pragma interface' means that unless this file (of the form "foo.h"
   is not presently being included by file "foo.cc", the
   CLASSTYPE_INTERFACE_ONLY bit gets set.  The effect is that none
   of the vtables nor any of the inline functions defined in foo.h
   will ever be output.

   There are cases when we want to link files such as "defs.h" and
   "main.cc".  In this case, we give "defs.h" a `#pragma interface',
   and "main.cc" has `#pragma implementation "defs.h"'.  */

struct impl_files
{
  const char *filename;
  struct impl_files *next;
};

static struct impl_files *impl_file_chain;

void
cxx_finish (void)
{
  c_common_finish ();
}

ovl_op_info_t ovl_op_info[2][OVL_OP_MAX] = 
  {
    {
      {NULL_TREE, NULL, NULL, ERROR_MARK, OVL_OP_ERROR_MARK, 0},
      {NULL_TREE, NULL, NULL, NOP_EXPR, OVL_OP_NOP_EXPR, 0},
#define DEF_OPERATOR(NAME, CODE, MANGLING, FLAGS) \
      {NULL_TREE, NAME, MANGLING, CODE, OVL_OP_##CODE, FLAGS},
#define OPERATOR_TRANSITION }, {			\
      {NULL_TREE, NULL, NULL, ERROR_MARK, OVL_OP_ERROR_MARK, 0},
#include "operators.def"
    }
  };
unsigned char ovl_op_mapping[MAX_TREE_CODES];
unsigned char ovl_op_alternate[OVL_OP_MAX];

/* Get the name of the kind of identifier T.  */

const char *
get_identifier_kind_name (tree id)
{
  /* Keep in sync with cp_id_kind enumeration.  */
  static const char *const names[cik_max] = {
    "normal", "keyword", "constructor", "destructor",
    "simple-op", "assign-op", "conv-op", "<reserved>udlit-op"
  };

  unsigned kind = 0;
  kind |= IDENTIFIER_KIND_BIT_2 (id) << 2;
  kind |= IDENTIFIER_KIND_BIT_1 (id) << 1;
  kind |= IDENTIFIER_KIND_BIT_0 (id) << 0;

  return names[kind];
}

/* Set the identifier kind, which we expect to currently be zero.  */

void
set_identifier_kind (tree id, cp_identifier_kind kind)
{
  gcc_checking_assert (!IDENTIFIER_KIND_BIT_2 (id)
		       & !IDENTIFIER_KIND_BIT_1 (id)
		       & !IDENTIFIER_KIND_BIT_0 (id));
  IDENTIFIER_KIND_BIT_2 (id) |= (kind >> 2) & 1;
  IDENTIFIER_KIND_BIT_1 (id) |= (kind >> 1) & 1;
  IDENTIFIER_KIND_BIT_0 (id) |= (kind >> 0) & 1;
}

/* Create and tag the internal operator name for the overloaded
   operator PTR describes.  */

static tree
set_operator_ident (ovl_op_info_t *ptr)
{
  char buffer[32];
  size_t len = snprintf (buffer, sizeof (buffer), "operator%s%s",
			 &" "[ptr->name[0] && ptr->name[0] != '_'
			      && !ISALPHA (ptr->name[0])],
			 ptr->name);
  gcc_checking_assert (len < sizeof (buffer));

  tree ident = get_identifier_with_length (buffer, len);
  ptr->identifier = ident;

  return ident;
}

/* Initialize data structures that keep track of operator names.  */

static void
init_operators (void)
{
  /* We rely on both these being zero.  */
  gcc_checking_assert (!OVL_OP_ERROR_MARK && !ERROR_MARK);

  /* This loop iterates backwards because we need to move the
     assignment operators down to their correct slots.  I.e. morally
     equivalent to an overlapping memmove where dest > src.  Slot
     zero is for error_mark, so hae no operator. */
  for (unsigned ix = OVL_OP_MAX; --ix;)
    {
      ovl_op_info_t *op_ptr = &ovl_op_info[false][ix];

      if (op_ptr->name)
	{
	  tree ident = set_operator_ident (op_ptr);
	  if (unsigned index = IDENTIFIER_CP_INDEX (ident))
	    {
	      ovl_op_info_t *bin_ptr = &ovl_op_info[false][index];

	      /* They should only differ in unary/binary ness.  */
	      gcc_checking_assert ((op_ptr->flags ^ bin_ptr->flags)
				   == OVL_OP_FLAG_AMBIARY);
	      bin_ptr->flags |= op_ptr->flags;
	      ovl_op_alternate[index] = ix;
	    }
	  else
	    {
	      IDENTIFIER_CP_INDEX (ident) = ix;
	      set_identifier_kind (ident, cik_simple_op);
	    }
	}
      if (op_ptr->tree_code)
	{
	  gcc_checking_assert (op_ptr->ovl_op_code == ix
			       && !ovl_op_mapping[op_ptr->tree_code]);
	  ovl_op_mapping[op_ptr->tree_code] = op_ptr->ovl_op_code;
	}

      ovl_op_info_t *as_ptr = &ovl_op_info[true][ix];
      if (as_ptr->name)
	{
	  /* These will be placed at the start of the array, move to
	     the correct slot and initialize.  */
	  if (as_ptr->ovl_op_code != ix)
	    {
	      ovl_op_info_t *dst_ptr = &ovl_op_info[true][as_ptr->ovl_op_code];
	      gcc_assert (as_ptr->ovl_op_code > ix && !dst_ptr->tree_code);
	      memcpy (dst_ptr, as_ptr, sizeof (*dst_ptr));
	      memset (as_ptr, 0, sizeof (*as_ptr));
	      as_ptr = dst_ptr;
	    }

	  tree ident = set_operator_ident (as_ptr);
	  gcc_checking_assert (!IDENTIFIER_CP_INDEX (ident));
	  IDENTIFIER_CP_INDEX (ident) = as_ptr->ovl_op_code;
	  set_identifier_kind (ident, cik_assign_op);

	  gcc_checking_assert (!ovl_op_mapping[as_ptr->tree_code]
			       || (ovl_op_mapping[as_ptr->tree_code]
				   == as_ptr->ovl_op_code));
	  ovl_op_mapping[as_ptr->tree_code] = as_ptr->ovl_op_code;
	}
    }
}

/* Initialize the reserved words.  */

void
init_reswords (void)
{
  unsigned int i;
  tree id;
  int mask = 0;

  if (cxx_dialect < cxx11)
    mask |= D_CXX11;
  if (cxx_dialect < cxx20)
    mask |= D_CXX20;
  if (!flag_concepts)
    mask |= D_CXX_CONCEPTS;
  if (!flag_coroutines)
    mask |= D_CXX_COROUTINES;
  if (!flag_modules)
    mask |= D_CXX_MODULES;
  if (!flag_tm)
    mask |= D_TRANSMEM;
  if (!flag_char8_t)
    mask |= D_CXX_CHAR8_T;
  if (flag_no_asm)
    mask |= D_ASM | D_EXT | D_EXT11;
  if (flag_no_gnu_keywords)
    mask |= D_EXT | D_EXT11;

  /* The Objective-C keywords are all context-dependent.  */
  mask |= D_OBJC;

  ridpointers = ggc_cleared_vec_alloc<tree> ((int) RID_MAX);
  for (i = 0; i < num_c_common_reswords; i++)
    {
      if (c_common_reswords[i].disable & D_CONLY)
	continue;
      id = get_identifier (c_common_reswords[i].word);
      C_SET_RID_CODE (id, c_common_reswords[i].rid);
      ridpointers [(int) c_common_reswords[i].rid] = id;
      if (! (c_common_reswords[i].disable & mask))
	set_identifier_kind (id, cik_keyword);
    }

  for (i = 0; i < NUM_INT_N_ENTS; i++)
    {
      char name[50];
      sprintf (name, "__int%d", int_n_data[i].bitsize);
      id = get_identifier (name);
      C_SET_RID_CODE (id, RID_FIRST_INT_N + i);
      set_identifier_kind (id, cik_keyword);

      sprintf (name, "__int%d__", int_n_data[i].bitsize);
      id = get_identifier (name);
      C_SET_RID_CODE (id, RID_FIRST_INT_N + i);
      set_identifier_kind (id, cik_keyword);
    }

  if (flag_openmp)
    {
      id = get_identifier ("omp_all_memory");
      C_SET_RID_CODE (id, RID_OMP_ALL_MEMORY);
      set_identifier_kind (id, cik_keyword);
      ridpointers [RID_OMP_ALL_MEMORY] = id;
    }
}

static void
init_cp_pragma (void)
{
  c_register_pragma (0, "vtable", handle_pragma_vtable);
  c_register_pragma (0, "unit", handle_pragma_unit);
  c_register_pragma (0, "interface", handle_pragma_interface);
  c_register_pragma (0, "implementation", handle_pragma_implementation);
  c_register_pragma ("GCC", "interface", handle_pragma_interface);
  c_register_pragma ("GCC", "implementation", handle_pragma_implementation);
}

/* TRUE if a code represents a statement.  */

bool statement_code_p[MAX_TREE_CODES];

/* Initialize the C++ front end.  This function is very sensitive to
   the exact order that things are done here.  It would be nice if the
   initialization done by this routine were moved to its subroutines,
   and the ordering dependencies clarified and reduced.  */
bool
cxx_init (void)
{
  location_t saved_loc;
  unsigned int i;
  static const enum tree_code stmt_codes[] = {
   CTOR_INITIALIZER,	TRY_BLOCK,	HANDLER,
   EH_SPEC_BLOCK,	USING_STMT,	TAG_DEFN,
   IF_STMT,		CLEANUP_STMT,	FOR_STMT,
   RANGE_FOR_STMT,	WHILE_STMT,	DO_STMT,
   BREAK_STMT,		CONTINUE_STMT,	SWITCH_STMT,
   EXPR_STMT,		OMP_DEPOBJ
  };

  memset (&statement_code_p, 0, sizeof (statement_code_p));
  for (i = 0; i < ARRAY_SIZE (stmt_codes); i++)
    statement_code_p[stmt_codes[i]] = true;

  saved_loc = input_location;
  input_location = BUILTINS_LOCATION;

  init_reswords ();
  init_tree ();
  init_cp_semantics ();
  init_operators ();
  init_method ();

  current_function_decl = NULL;

  class_type_node = ridpointers[(int) RID_CLASS];

  cxx_init_decl_processing ();

  if (c_common_init () == false)
    {
      input_location = saved_loc;
      return false;
    }

  init_cp_pragma ();

  input_location = saved_loc;
  return true;
}

/* Return nonzero if S is not considered part of an
   INTERFACE/IMPLEMENTATION pair.  Otherwise, return 0.  */

static int
interface_strcmp (const char* s)
{
  /* Set the interface/implementation bits for this scope.  */
  struct impl_files *ifiles;
  const char *s1;

  for (ifiles = impl_file_chain; ifiles; ifiles = ifiles->next)
    {
      const char *t1 = ifiles->filename;
      s1 = s;

      if (*s1 == 0 || filename_ncmp (s1, t1, 1) != 0)
	continue;

      while (*s1 != 0 && filename_ncmp (s1, t1, 1) == 0)
	s1++, t1++;

      /* A match.  */
      if (*s1 == *t1)
	return 0;

      /* Don't get faked out by xxx.yyy.cc vs xxx.zzz.cc.  */
      if (strchr (s1, '.') || strchr (t1, '.'))
	continue;

      if (*s1 == '\0' || s1[-1] != '.' || t1[-1] != '.')
	continue;

      /* A match.  */
      return 0;
    }

  /* No matches.  */
  return 1;
}

/* We've just read a cpp-token, figure out our next state.  Hey, this
   is a hand-coded co-routine!  */

struct module_token_filter
{
  enum state
  {
   idle,
   module_first,
   module_cont,
   module_end,
  };

  enum state state : 8;
  bool is_import : 1;
  bool got_export : 1;
  bool got_colon : 1;
  bool want_dot : 1;

  location_t token_loc;
  cpp_reader *reader;
  module_state *module;
  module_state *import;

  module_token_filter (cpp_reader *reader)
    : state (idle), is_import (false),
    got_export (false), got_colon (false), want_dot (false),
    token_loc (UNKNOWN_LOCATION),
    reader (reader), module (NULL), import (NULL)
  {
  };

  /* Process the next token.  Note we cannot see CPP_EOF inside a
     pragma -- a CPP_PRAGMA_EOL always happens.  */
  uintptr_t resume (int type, int keyword, tree value, location_t loc)
  {
    unsigned res = 0;

    switch (state)
      {
      case idle:
	if (type == CPP_KEYWORD)
	  switch (keyword)
	    {
	    default:
	      break;

	    case RID__EXPORT:
	      got_export = true;
	      res = lang_hooks::PT_begin_pragma;
	      break;

	    case RID__IMPORT:
	      is_import = true;
	      /* FALLTHRU */
	    case RID__MODULE:
	      state = module_first;
	      want_dot = false;
	      got_colon = false;
	      token_loc = loc;
	      import = NULL;
	      if (!got_export)
		res = lang_hooks::PT_begin_pragma;
	      break;
	    }
	break;

      case module_first:
	if (is_import && type == CPP_HEADER_NAME)
	  {
	    /* A header name.  The preprocessor will have already
	       done include searching and canonicalization.  */
	    state = module_end;
	    goto header_unit;
	  }
	
	if (type == CPP_PADDING || type == CPP_COMMENT)
	  break;

	state = module_cont;
	if (type == CPP_COLON && module)
	  {
	    got_colon = true;
	    import = module;
	    break;
	  }
	/* FALLTHROUGH  */

      case module_cont:
	switch (type)
	  {
	  case CPP_PADDING:
	  case CPP_COMMENT:
	    break;

	  default:
	    /* If we ever need to pay attention to attributes for
	       header modules, more logic will be needed.  */
	    state = module_end;
	    break;

	  case CPP_COLON:
	    if (got_colon)
	      state = module_end;
	    got_colon = true;
	    /* FALLTHROUGH  */
	  case CPP_DOT:
	    if (!want_dot)
	      state = module_end;
	    want_dot = false;
	    break;

	  case CPP_PRAGMA_EOL:
	    goto module_end;

	  case CPP_NAME:
	    if (want_dot)
	      {
		/* Got name instead of [.:].  */
		state = module_end;
		break;
	      }
	  header_unit:
	    import = get_module (value, import, got_colon);
	    want_dot = true;
	    break;
	  }
	break;

      case module_end:
	if (type == CPP_PRAGMA_EOL)
	  {
	  module_end:;
	    /* End of the directive, handle the name.  */
	    if (import && (is_import || !flag_header_unit))
	      if (module_state *m
		  = preprocess_module (import, token_loc, module != NULL,
				       is_import, got_export, reader))
		if (!module)
		  module = m;

	    is_import = got_export = false;
	    state = idle;
	  }
	break;
      }

    return res;
  }
};

/* Initialize or teardown.  */

uintptr_t
module_token_cdtor (cpp_reader *pfile, uintptr_t data_)
{
  if (module_token_filter *filter = reinterpret_cast<module_token_filter *> (data_))
    {
      preprocessed_module (pfile);
      delete filter;
      data_ = 0;
    }
  else if (modules_p ())
    data_ = reinterpret_cast<uintptr_t > (new module_token_filter (pfile));

  return data_;
}

uintptr_t
module_token_lang (int type, int keyword, tree value, location_t loc,
		   uintptr_t data_)
{
  module_token_filter *filter = reinterpret_cast<module_token_filter *> (data_);
  return filter->resume (type, keyword, value, loc);
}

uintptr_t
module_token_pre (cpp_reader *pfile, const cpp_token *tok, uintptr_t data_)
{
  if (!tok)
    return module_token_cdtor (pfile, data_);

  int type = tok->type;
  int keyword = RID_MAX;
  tree value = NULL_TREE;

  if (tok->type == CPP_NAME)
    {
      value = HT_IDENT_TO_GCC_IDENT (HT_NODE (tok->val.node.node));
      if (IDENTIFIER_KEYWORD_P (value))
	{
	  keyword = C_RID_CODE (value);
	  type = CPP_KEYWORD;
	}
    }
  else if (tok->type == CPP_HEADER_NAME)
    value = build_string (tok->val.str.len, (const char *)tok->val.str.text);

  return module_token_lang (type, keyword, value, tok->src_loc, data_);
}

/* Parse a #pragma whose sole argument is a string constant.
   If OPT is true, the argument is optional.  */
static tree
parse_strconst_pragma (const char* name, int opt)
{
  tree result, x;
  enum cpp_ttype t;

  t = pragma_lex (&result);
  if (t == CPP_STRING)
    {
      if (pragma_lex (&x) != CPP_EOF)
	warning (0, "junk at end of %<#pragma %s%>", name);
      return result;
    }

  if (t == CPP_EOF && opt)
    return NULL_TREE;

  error ("invalid %<#pragma %s%>", name);
  return error_mark_node;
}

static void
handle_pragma_vtable (cpp_reader* /*dfile*/)
{
  parse_strconst_pragma ("vtable", 0);
  sorry ("%<#pragma vtable%> no longer supported");
}

static void
handle_pragma_unit (cpp_reader* /*dfile*/)
{
  /* Validate syntax, but don't do anything.  */
  parse_strconst_pragma ("unit", 0);
}

static void
handle_pragma_interface (cpp_reader* /*dfile*/)
{
  tree fname = parse_strconst_pragma ("interface", 1);
  struct c_fileinfo *finfo;
  const char *filename;

  if (fname == error_mark_node)
    return;
  else if (fname == 0)
    filename = lbasename (LOCATION_FILE (input_location));
  else
    filename = TREE_STRING_POINTER (fname);

  finfo = get_fileinfo (LOCATION_FILE (input_location));

  if (impl_file_chain == 0)
    {
      /* If this is zero at this point, then we are
	 auto-implementing.  */
      if (main_input_filename == 0)
	main_input_filename = LOCATION_FILE (input_location);
    }

  finfo->interface_only = interface_strcmp (filename);
  /* If MULTIPLE_SYMBOL_SPACES is set, we cannot assume that we can see
     a definition in another file.  */
  if (!MULTIPLE_SYMBOL_SPACES || !finfo->interface_only)
    finfo->interface_unknown = 0;
}

/* Note that we have seen a #pragma implementation for the key MAIN_FILENAME.
   We used to only allow this at toplevel, but that restriction was buggy
   in older compilers and it seems reasonable to allow it in the headers
   themselves, too.  It only needs to precede the matching #p interface.

   We don't touch finfo->interface_only or finfo->interface_unknown;
   the user must specify a matching #p interface for this to have
   any effect.  */

static void
handle_pragma_implementation (cpp_reader* /*dfile*/)
{
  tree fname = parse_strconst_pragma ("implementation", 1);
  const char *filename;
  struct impl_files *ifiles = impl_file_chain;

  if (fname == error_mark_node)
    return;

  if (fname == 0)
    {
      if (main_input_filename)
	filename = main_input_filename;
      else
	filename = LOCATION_FILE (input_location);
      filename = lbasename (filename);
    }
  else
    {
      filename = TREE_STRING_POINTER (fname);
      if (cpp_included_before (parse_in, filename, input_location))
	warning (0, "%<#pragma implementation%> for %qs appears after "
		 "file is included", filename);
    }

  for (; ifiles; ifiles = ifiles->next)
    {
      if (! filename_cmp (ifiles->filename, filename))
	break;
    }
  if (ifiles == 0)
    {
      ifiles = XNEW (struct impl_files);
      ifiles->filename = xstrdup (filename);
      ifiles->next = impl_file_chain;
      impl_file_chain = ifiles;
    }
}

/* Issue an error message indicating that the lookup of NAME (an
   IDENTIFIER_NODE) failed.  Returns the ERROR_MARK_NODE.  */

tree
unqualified_name_lookup_error (tree name, location_t loc)
{
  if (loc == UNKNOWN_LOCATION)
    loc = cp_expr_loc_or_input_loc (name);

  if (IDENTIFIER_ANY_OP_P (name))
    error_at (loc, "%qD not defined", name);
  else
    {
      if (!objc_diagnose_private_ivar (name))
	{
	  auto_diagnostic_group d;
	  name_hint hint = suggest_alternatives_for (loc, name, true);
	  if (const char *suggestion = hint.suggestion ())
	    {
	      gcc_rich_location richloc (loc);
	      richloc.add_fixit_replace (suggestion);
	      error_at (&richloc,
			"%qD was not declared in this scope; did you mean %qs?",
			name, suggestion);
	    }
	  else
	    error_at (loc, "%qD was not declared in this scope", name);
	}
      /* Prevent repeated error messages by creating a VAR_DECL with
	 this NAME in the innermost block scope.  */
      if (local_bindings_p ())
	{
	  tree decl = build_decl (loc, VAR_DECL, name, error_mark_node);
	  TREE_USED (decl) = true;
	  pushdecl (decl);
	}
    }

  return error_mark_node;
}

/* Like unqualified_name_lookup_error, but NAME_EXPR is an unqualified-id
   NAME, encapsulated with its location in a CP_EXPR, used as a function.
   Returns an appropriate expression for NAME.  */

tree
unqualified_fn_lookup_error (cp_expr name_expr)
{
  tree name = name_expr.get_value ();
  location_t loc = name_expr.get_location ();
  if (loc == UNKNOWN_LOCATION)
    loc = input_location;

  if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
    name = TREE_OPERAND (name, 0);

  if (processing_template_decl)
    {
      /* In a template, it is invalid to write "f()" or "f(3)" if no
	 declaration of "f" is available.  Historically, G++ and most
	 other compilers accepted that usage since they deferred all name
	 lookup until instantiation time rather than doing unqualified
	 name lookup at template definition time; explain to the user what
	 is going wrong.

	 Note that we have the exact wording of the following message in
	 the manual (trouble.texi, node "Name lookup"), so they need to
	 be kept in synch.  */
      permerror (loc, "there are no arguments to %qD that depend on a template "
		 "parameter, so a declaration of %qD must be available",
		 name, name);

      if (!flag_permissive)
	{
	  static bool hint;
	  if (!hint)
	    {
	      inform (loc, "(if you use %<-fpermissive%>, G++ will accept your "
		      "code, but allowing the use of an undeclared name is "
		      "deprecated)");
	      hint = true;
	    }
	}
      return name;
    }

  return unqualified_name_lookup_error (name, loc);
}


/* Hasher for the conversion operator name hash table.  */
struct conv_type_hasher : ggc_ptr_hash<tree_node>
{
  /* Hash NODE, an identifier node in the table.  TYPE_UID is
     suitable, as we're not concerned about matching canonicalness
     here.  */
  static hashval_t hash (tree node)
  {
    return (hashval_t) TYPE_UID (TREE_TYPE (node));
  }

  /* Compare NODE, an identifier node in the table, against TYPE, an
     incoming TYPE being looked up.  */
  static bool equal (tree node, tree type)
  {
    return TREE_TYPE (node) == type;
  }
};

/* This hash table maps TYPEs to the IDENTIFIER for a conversion
   operator to TYPE.  The nodes are IDENTIFIERs whose TREE_TYPE is the
   TYPE.  */

static GTY (()) hash_table<conv_type_hasher> *conv_type_names;

/* Return an identifier for a conversion operator to TYPE.  We can get
   from the returned identifier to the type.  We store TYPE, which is
   not necessarily the canonical type,  which allows us to report the
   form the user used in error messages.  All these identifiers are
   not in the identifier hash table, and have the same string name.
   These IDENTIFIERS are not in the identifier hash table, and all
   have the same IDENTIFIER_STRING.  */

tree
make_conv_op_name (tree type)
{
  if (type == error_mark_node)
    return error_mark_node;

  if (conv_type_names == NULL)
    conv_type_names = hash_table<conv_type_hasher>::create_ggc (31);

  tree *slot = conv_type_names->find_slot_with_hash
    (type, (hashval_t) TYPE_UID (type), INSERT);
  tree identifier = *slot;
  if (!identifier)
    {
      /* Create a raw IDENTIFIER outside of the identifier hash
	 table.  */
      identifier = copy_node (conv_op_identifier);

      /* Just in case something managed to bind.  */
      IDENTIFIER_BINDING (identifier) = NULL;

      /* Hang TYPE off the identifier so it can be found easily later
	 when performing conversions.  */
      TREE_TYPE (identifier) = type;

      *slot = identifier;
    }

  return identifier;
}

/* Wrapper around build_lang_decl_loc(). Should gradually move to
   build_lang_decl_loc() and then rename build_lang_decl_loc() back to
   build_lang_decl().  */

tree
build_lang_decl (enum tree_code code, tree name, tree type)
{
  return build_lang_decl_loc (input_location, code, name, type);
}

/* Build a decl from CODE, NAME, TYPE declared at LOC, and then add
   DECL_LANG_SPECIFIC info to the result.  */

tree
build_lang_decl_loc (location_t loc, enum tree_code code, tree name, tree type)
{
  tree t;

  t = build_decl (loc, code, name, type);
  retrofit_lang_decl (t);

  return t;
}

/* Maybe add a raw lang_decl to T, a decl.  Return true if it needed
   one.  */

bool
maybe_add_lang_decl_raw (tree t, bool decomp_p)
{
  size_t size;
  lang_decl_selector sel;

  if (decomp_p)
    sel = lds_decomp, size = sizeof (struct lang_decl_decomp);
  else if (TREE_CODE (t) == FUNCTION_DECL)
    sel = lds_fn, size = sizeof (struct lang_decl_fn);
  else if (TREE_CODE (t) == NAMESPACE_DECL)
    sel = lds_ns, size = sizeof (struct lang_decl_ns);
  else if (TREE_CODE (t) == PARM_DECL)
    sel = lds_parm, size = sizeof (struct lang_decl_parm);
  else if (LANG_DECL_HAS_MIN (t))
    sel = lds_min, size = sizeof (struct lang_decl_min);
  else
    return false;

  struct lang_decl *ld
    = (struct lang_decl *) ggc_internal_cleared_alloc (size);

  ld->u.base.selector = sel;
  DECL_LANG_SPECIFIC (t) = ld;

  if (sel == lds_ns)
    /* Who'd create a namespace, only to put nothing in it?  */
    ld->u.ns.bindings = hash_table<named_decl_hash>::create_ggc (499);

  if (GATHER_STATISTICS)
    {
      tree_node_counts[(int)lang_decl] += 1;
      tree_node_sizes[(int)lang_decl] += size;
    }
  return true;
}

/* T has just had a decl_lang_specific added.  Initialize its
   linkage.  */

static void
set_decl_linkage (tree t)
{
  if (current_lang_name == lang_name_cplusplus
      || decl_linkage (t) == lk_none)
    SET_DECL_LANGUAGE (t, lang_cplusplus);
  else if (current_lang_name == lang_name_c)
    SET_DECL_LANGUAGE (t, lang_c);
  else
    gcc_unreachable ();
}

/* T is a VAR_DECL node that needs to be a decomposition of BASE.  */

void
fit_decomposition_lang_decl (tree t, tree base)
{
  if (struct lang_decl *orig_ld = DECL_LANG_SPECIFIC (t))
    {
      if (orig_ld->u.base.selector == lds_min)
	{
	  maybe_add_lang_decl_raw (t, true);
	  memcpy (DECL_LANG_SPECIFIC (t), orig_ld,
		  sizeof (struct lang_decl_min));
	  /* Reset selector, which will have been bashed by the
	     memcpy.  */
	  DECL_LANG_SPECIFIC (t)->u.base.selector = lds_decomp;
	}
      else
	gcc_checking_assert (orig_ld->u.base.selector == lds_decomp);
    }
  else
    {
      maybe_add_lang_decl_raw (t, true);
      set_decl_linkage (t);
    }

  DECL_DECOMP_BASE (t) = base;
}

/* Add DECL_LANG_SPECIFIC info to T, if it needs one.  Generally
   every C++ decl needs one, but C builtins etc do not.   */

void
retrofit_lang_decl (tree t)
{
  if (DECL_LANG_SPECIFIC (t))
    return;

  if (maybe_add_lang_decl_raw (t, false))
    set_decl_linkage (t);
}

void
cxx_dup_lang_specific_decl (tree node)
{
  int size;

  if (! DECL_LANG_SPECIFIC (node))
    return;

  switch (DECL_LANG_SPECIFIC (node)->u.base.selector)
    {
    case lds_min:
      size = sizeof (struct lang_decl_min);
      break;
    case lds_fn:
      size = sizeof (struct lang_decl_fn);
      break;
    case lds_ns:
      size = sizeof (struct lang_decl_ns);
      break;
    case lds_parm:
      size = sizeof (struct lang_decl_parm);
      break;
    case lds_decomp:
      size = sizeof (struct lang_decl_decomp);
      break;
    default:
      gcc_unreachable ();
    }

  struct lang_decl *ld = (struct lang_decl *) ggc_internal_alloc (size);
  memcpy (ld, DECL_LANG_SPECIFIC (node), size);
  DECL_LANG_SPECIFIC (node) = ld;

  /* Directly clear some flags that do not apply to the copy
     (module_purview_p still does).  */
  ld->u.base.module_entity_p = false;
  ld->u.base.module_import_p = false;
  ld->u.base.module_keyed_decls_p = false;

  if (GATHER_STATISTICS)
    {
      tree_node_counts[(int)lang_decl] += 1;
      tree_node_sizes[(int)lang_decl] += size;
    }
}

/* Copy DECL, including any language-specific parts.  */

tree
copy_decl (tree decl MEM_STAT_DECL)
{
  tree copy;

  copy = copy_node (decl PASS_MEM_STAT);
  cxx_dup_lang_specific_decl (copy);
  return copy;
}

/* Replace the shared language-specific parts of NODE with a new copy.  */

static void
copy_lang_type (tree node)
{
  if (! TYPE_LANG_SPECIFIC (node))
    return;

  auto *lt = (struct lang_type *) ggc_internal_alloc (sizeof (struct lang_type));

  memcpy (lt, TYPE_LANG_SPECIFIC (node), (sizeof (struct lang_type)));
  TYPE_LANG_SPECIFIC (node) = lt;

  if (GATHER_STATISTICS)
    {
      tree_node_counts[(int)lang_type] += 1;
      tree_node_sizes[(int)lang_type] += sizeof (struct lang_type);
    }
}

/* Copy TYPE, including any language-specific parts.  */

tree
copy_type (tree type MEM_STAT_DECL)
{
  tree copy;

  copy = copy_node (type PASS_MEM_STAT);
  copy_lang_type (copy);
  return copy;
}

/* Add a raw lang_type to T, a type, should it need one.  */

bool
maybe_add_lang_type_raw (tree t)
{
  if (!RECORD_OR_UNION_CODE_P (TREE_CODE (t)))
    return false;
  
  auto *lt = (struct lang_type *) (ggc_internal_cleared_alloc
				   (sizeof (struct lang_type)));
  TYPE_LANG_SPECIFIC (t) = lt;

  if (GATHER_STATISTICS)
    {
      tree_node_counts[(int)lang_type] += 1;
      tree_node_sizes[(int)lang_type] += sizeof (struct lang_type);
    }

  return true;
}

tree
cxx_make_type (enum tree_code code MEM_STAT_DECL)
{
  tree t = make_node (code PASS_MEM_STAT);

  if (maybe_add_lang_type_raw (t))
    {
      /* Set up some flags that give proper default behavior.  */
      struct c_fileinfo *finfo =
	get_fileinfo (LOCATION_FILE (input_location));
      SET_CLASSTYPE_INTERFACE_UNKNOWN_X (t, finfo->interface_unknown);
      CLASSTYPE_INTERFACE_ONLY (t) = finfo->interface_only;
    }

  if (code == RECORD_TYPE || code == UNION_TYPE)
    TYPE_CXX_ODR_P (t) = 1;

  return t;
}

/* A wrapper without the memory stats for LANG_HOOKS_MAKE_TYPE.  */

tree
cxx_make_type_hook (enum tree_code code)
{
  return cxx_make_type (code);
}

tree
make_class_type (enum tree_code code MEM_STAT_DECL)
{
  tree t = cxx_make_type (code PASS_MEM_STAT);
  SET_CLASS_TYPE_P (t, 1);
  return t;
}

/* Returns true if we are currently in the main source file, or in a
   template instantiation started from the main source file.  */

bool
in_main_input_context (void)
{
  struct tinst_level *tl = outermost_tinst_level();

  if (tl)
    return filename_cmp (main_input_filename,
			 LOCATION_FILE (tl->locus)) == 0;
  else
    return filename_cmp (main_input_filename, LOCATION_FILE (input_location)) == 0;
}

#include "gt-cp-lex.h"
