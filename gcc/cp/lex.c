/* Separate lexical analyzer for GNU C++.
   Copyright (C) 1987-2017 Free Software Foundation, Inc.
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
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "c-family/c-pragma.h"
#include "c-family/c-objc.h"

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

ovl_op_info_t ovl_op_info[2][MAX_TREE_CODES];

/* Get the name of the kind of identifier T.  */

const char *
get_identifier_kind_name (tree id)
{
  /* Keep in sync with cp_id_kind enumeration.  */
  static const char *const names[cik_max] = {
    "normal", "keyword", "constructor", "destructor",
    "assign-op", "op-assign-op", "simple-op", "conv-op", };

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

static void
init_operators (void)
{
  tree identifier;
  ovl_op_info_t *oni;

#define DEF_OPERATOR(NAME, CODE, MANGLING, FLAGS, KIND)			\
  oni = OVL_OP_INFO (KIND == cik_assign_op, CODE);			\
  oni->name = NAME;							\
  oni->mangled_name = MANGLING;						\
  oni->tree_code = CODE;						\
  oni->flags = FLAGS;							\
  if (NAME) {								\
    identifier = set_operator_ident (oni);				\
    if (KIND != cik_simple_op || !IDENTIFIER_ANY_OP_P (identifier))	\
      set_identifier_kind (identifier, KIND);				\
  }

#include "operators.def"
#undef DEF_OPERATOR
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
  if (!flag_concepts)
    mask |= D_CXX_CONCEPTS;
  if (!flag_tm)
    mask |= D_TRANSMEM;
  if (flag_no_asm)
    mask |= D_ASM | D_EXT;
  if (flag_no_gnu_keywords)
    mask |= D_EXT;

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
   EXPR_STMT
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

  init_repo ();

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
	warning (0, "junk at end of #pragma %s", name);
      return result;
    }

  if (t == CPP_EOF && opt)
    return NULL_TREE;

  error ("invalid #pragma %s", name);
  return error_mark_node;
}

static void
handle_pragma_vtable (cpp_reader* /*dfile*/)
{
  parse_strconst_pragma ("vtable", 0);
  sorry ("#pragma vtable no longer supported");
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
	warning (0, "#pragma implementation for %qs appears after "
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
    loc = EXPR_LOC_OR_LOC (name, input_location);

  if (IDENTIFIER_ANY_OP_P (name))
    error_at (loc, "%qD not defined", name);
  else
    {
      if (!objc_diagnose_private_ivar (name))
	{
	  error_at (loc, "%qD was not declared in this scope", name);
	  suggest_alternatives_for (loc, name, true);
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

static bool
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

  struct lang_type *lt
    = (struct lang_type *) ggc_internal_alloc (sizeof (struct lang_type));

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

static bool
maybe_add_lang_type_raw (tree t)
{
  if (!RECORD_OR_UNION_CODE_P (TREE_CODE (t)))
    return false;
  
  TYPE_LANG_SPECIFIC (t)
    = (struct lang_type *) (ggc_internal_cleared_alloc
			    (sizeof (struct lang_type)));

  if (GATHER_STATISTICS)
    {
      tree_node_counts[(int)lang_type] += 1;
      tree_node_sizes[(int)lang_type] += sizeof (struct lang_type);
    }

  return true;
}

tree
cxx_make_type (enum tree_code code)
{
  tree t = make_node (code);

  if (maybe_add_lang_type_raw (t))
    {
      /* Set up some flags that give proper default behavior.  */
      struct c_fileinfo *finfo =
	get_fileinfo (LOCATION_FILE (input_location));
      SET_CLASSTYPE_INTERFACE_UNKNOWN_X (t, finfo->interface_unknown);
      CLASSTYPE_INTERFACE_ONLY (t) = finfo->interface_only;
    }

  return t;
}

tree
make_class_type (enum tree_code code)
{
  tree t = cxx_make_type (code);
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
