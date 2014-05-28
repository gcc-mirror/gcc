/* Name mangling for the 3.0 C++ ABI.
   Copyright (C) 2000-2014 Free Software Foundation, Inc.
   Written by Alex Samuel <samuel@codesourcery.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This file implements mangling of C++ names according to the IA64
   C++ ABI specification.  A mangled name encodes a function or
   variable's name, scope, type, and/or template arguments into a text
   identifier.  This identifier is used as the function's or
   variable's linkage name, to preserve compatibility between C++'s
   language features (templates, scoping, and overloading) and C
   linkers.

   Additionally, g++ uses mangled names internally.  To support this,
   mangling of types is allowed, even though the mangled name of a
   type should not appear by itself as an exported name.  Ditto for
   uninstantiated templates.

   The primary entry point for this module is mangle_decl, which
   returns an identifier containing the mangled name for a decl.
   Additional entry points are provided to build mangled names of
   particular constructs when the appropriate decl for that construct
   is not available.  These are:

     mangle_typeinfo_for_type:		typeinfo data
     mangle_typeinfo_string_for_type:	typeinfo type name
     mangle_vtbl_for_type:		virtual table data
     mangle_vtt_for_type:		VTT data
     mangle_ctor_vtbl_for_type:		`C-in-B' constructor virtual table data
     mangle_thunk:			thunk function or entry  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stor-layout.h"
#include "stringpool.h"
#include "tm_p.h"
#include "cp-tree.h"
#include "obstack.h"
#include "flags.h"
#include "target.h"
#include "cgraph.h"
#include "wide-int.h"

/* Debugging support.  */

/* Define DEBUG_MANGLE to enable very verbose trace messages.  */
#ifndef DEBUG_MANGLE
#define DEBUG_MANGLE 0
#endif

/* Macros for tracing the write_* functions.  */
#if DEBUG_MANGLE
# define MANGLE_TRACE(FN, INPUT) \
  fprintf (stderr, "  %-24s: %-24s\n", (FN), (INPUT))
# define MANGLE_TRACE_TREE(FN, NODE) \
  fprintf (stderr, "  %-24s: %-24s (%p)\n", \
	   (FN), get_tree_code_name (TREE_CODE (NODE)), (void *) (NODE))
#else
# define MANGLE_TRACE(FN, INPUT)
# define MANGLE_TRACE_TREE(FN, NODE)
#endif

/* Nonzero if NODE is a class template-id.  We can't rely on
   CLASSTYPE_USE_TEMPLATE here because of tricky bugs in the parser
   that hard to distinguish A<T> from A, where A<T> is the type as
   instantiated outside of the template, and A is the type used
   without parameters inside the template.  */
#define CLASSTYPE_TEMPLATE_ID_P(NODE)					\
  (TYPE_LANG_SPECIFIC (NODE) != NULL					\
   && (TREE_CODE (NODE) == BOUND_TEMPLATE_TEMPLATE_PARM			\
       || (CLASSTYPE_TEMPLATE_INFO (NODE) != NULL			\
	   && (PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (NODE))))))

/* Things we only need one of.  This module is not reentrant.  */
typedef struct GTY(()) globals {
  /* An array of the current substitution candidates, in the order
     we've seen them.  */
  vec<tree, va_gc> *substitutions;

  /* The entity that is being mangled.  */
  tree GTY ((skip)) entity;

  /* How many parameter scopes we are inside.  */
  int parm_depth;

  /* True if the mangling will be different in a future version of the
     ABI.  */
  bool need_abi_warning;
} globals;

static GTY (()) globals G;

/* The obstack on which we build mangled names.  */
static struct obstack *mangle_obstack;

/* The obstack on which we build mangled names that are not going to
   be IDENTIFIER_NODEs.  */
static struct obstack name_obstack;

/* The first object on the name_obstack; we use this to free memory
   allocated on the name_obstack.  */
static void *name_base;

/* Indices into subst_identifiers.  These are identifiers used in
   special substitution rules.  */
typedef enum
{
  SUBID_ALLOCATOR,
  SUBID_BASIC_STRING,
  SUBID_CHAR_TRAITS,
  SUBID_BASIC_ISTREAM,
  SUBID_BASIC_OSTREAM,
  SUBID_BASIC_IOSTREAM,
  SUBID_MAX
}
substitution_identifier_index_t;

/* For quick substitution checks, look up these common identifiers
   once only.  */
static GTY(()) tree subst_identifiers[SUBID_MAX];

/* Single-letter codes for builtin integer types, defined in
   <builtin-type>.  These are indexed by integer_type_kind values.  */
static const char
integer_type_codes[itk_none] =
{
  'c',  /* itk_char */
  'a',  /* itk_signed_char */
  'h',  /* itk_unsigned_char */
  's',  /* itk_short */
  't',  /* itk_unsigned_short */
  'i',  /* itk_int */
  'j',  /* itk_unsigned_int */
  'l',  /* itk_long */
  'm',  /* itk_unsigned_long */
  'x',  /* itk_long_long */
  'y',  /* itk_unsigned_long_long */
  'n',  /* itk_int128 */
  'o',  /* itk_unsigned_int128  */
};

static int decl_is_template_id (const tree, tree* const);

/* Functions for handling substitutions.  */

static inline tree canonicalize_for_substitution (tree);
static void add_substitution (tree);
static inline int is_std_substitution (const tree,
				       const substitution_identifier_index_t);
static inline int is_std_substitution_char (const tree,
					    const substitution_identifier_index_t);
static int find_substitution (tree);
static void mangle_call_offset (const tree, const tree);

/* Functions for emitting mangled representations of things.  */

static void write_mangled_name (const tree, bool);
static void write_encoding (const tree);
static void write_name (tree, const int);
static void write_abi_tags (tree);
static void write_unscoped_name (const tree);
static void write_unscoped_template_name (const tree);
static void write_nested_name (const tree);
static void write_prefix (const tree);
static void write_template_prefix (const tree);
static void write_unqualified_name (tree);
static void write_conversion_operator_name (const tree);
static void write_source_name (tree);
static void write_literal_operator_name (tree);
static void write_unnamed_type_name (const tree);
static void write_closure_type_name (const tree);
static int hwint_to_ascii (unsigned HOST_WIDE_INT, const unsigned int, char *,
			   const unsigned int);
static void write_number (unsigned HOST_WIDE_INT, const int,
			  const unsigned int);
static void write_compact_number (int num);
static void write_integer_cst (const tree);
static void write_real_cst (const tree);
static void write_identifier (const char *);
static void write_special_name_constructor (const tree);
static void write_special_name_destructor (const tree);
static void write_type (tree);
static int write_CV_qualifiers_for_type (const tree);
static void write_builtin_type (tree);
static void write_function_type (const tree);
static void write_bare_function_type (const tree, const int, const tree);
static void write_method_parms (tree, const int, const tree);
static void write_class_enum_type (const tree);
static void write_template_args (tree);
static void write_expression (tree);
static void write_template_arg_literal (const tree);
static void write_template_arg (tree);
static void write_template_template_arg (const tree);
static void write_array_type (const tree);
static void write_pointer_to_member_type (const tree);
static void write_template_param (const tree);
static void write_template_template_param (const tree);
static void write_substitution (const int);
static int discriminator_for_local_entity (tree);
static int discriminator_for_string_literal (tree, tree);
static void write_discriminator (const int);
static void write_local_name (tree, const tree, const tree);
static void dump_substitution_candidates (void);
static tree mangle_decl_string (const tree);
static int local_class_index (tree);

/* Control functions.  */

static inline void start_mangling (const tree);
static inline const char *finish_mangling (const bool);
static tree mangle_special_for_type (const tree, const char *);

/* Foreign language functions.  */

static void write_java_integer_type_codes (const tree);

/* Append a single character to the end of the mangled
   representation.  */
#define write_char(CHAR)						\
  obstack_1grow (mangle_obstack, (CHAR))

/* Append a sized buffer to the end of the mangled representation.  */
#define write_chars(CHAR, LEN)						\
  obstack_grow (mangle_obstack, (CHAR), (LEN))

/* Append a NUL-terminated string to the end of the mangled
   representation.  */
#define write_string(STRING)						\
  obstack_grow (mangle_obstack, (STRING), strlen (STRING))

/* Nonzero if NODE1 and NODE2 are both TREE_LIST nodes and have the
   same purpose (context, which may be a type) and value (template
   decl).  See write_template_prefix for more information on what this
   is used for.  */
#define NESTED_TEMPLATE_MATCH(NODE1, NODE2)				\
  (TREE_CODE (NODE1) == TREE_LIST					\
   && TREE_CODE (NODE2) == TREE_LIST					\
   && ((TYPE_P (TREE_PURPOSE (NODE1))					\
	&& same_type_p (TREE_PURPOSE (NODE1), TREE_PURPOSE (NODE2)))	\
       || TREE_PURPOSE (NODE1) == TREE_PURPOSE (NODE2))			\
   && TREE_VALUE (NODE1) == TREE_VALUE (NODE2))

/* Write out an unsigned quantity in base 10.  */
#define write_unsigned_number(NUMBER)					\
  write_number ((NUMBER), /*unsigned_p=*/1, 10)

/* If DECL is a template instance, return nonzero and, if
   TEMPLATE_INFO is non-NULL, set *TEMPLATE_INFO to its template info.
   Otherwise return zero.  */

static int
decl_is_template_id (const tree decl, tree* const template_info)
{
  if (TREE_CODE (decl) == TYPE_DECL)
    {
      /* TYPE_DECLs are handled specially.  Look at its type to decide
	 if this is a template instantiation.  */
      const tree type = TREE_TYPE (decl);

      if (CLASS_TYPE_P (type) && CLASSTYPE_TEMPLATE_ID_P (type))
	{
	  if (template_info != NULL)
	    /* For a templated TYPE_DECL, the template info is hanging
	       off the type.  */
	    *template_info = TYPE_TEMPLATE_INFO (type);
	  return 1;
	}
    }
  else
    {
      /* Check if this is a primary template.  */
      if (DECL_LANG_SPECIFIC (decl) != NULL
	  && DECL_USE_TEMPLATE (decl)
	  && PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (decl))
	  && TREE_CODE (decl) != TEMPLATE_DECL)
	{
	  if (template_info != NULL)
	    /* For most templated decls, the template info is hanging
	       off the decl.  */
	    *template_info = DECL_TEMPLATE_INFO (decl);
	  return 1;
	}
    }

  /* It's not a template id.  */
  return 0;
}

/* Produce debugging output of current substitution candidates.  */

static void
dump_substitution_candidates (void)
{
  unsigned i;
  tree el;

  fprintf (stderr, "  ++ substitutions  ");
  FOR_EACH_VEC_ELT (*G.substitutions, i, el)
    {
      const char *name = "???";

      if (i > 0)
	fprintf (stderr, "                    ");
      if (DECL_P (el))
	name = IDENTIFIER_POINTER (DECL_NAME (el));
      else if (TREE_CODE (el) == TREE_LIST)
	name = IDENTIFIER_POINTER (DECL_NAME (TREE_VALUE (el)));
      else if (TYPE_NAME (el))
	name = TYPE_NAME_STRING (el);
      fprintf (stderr, " S%d_ = ", i - 1);
      if (TYPE_P (el) &&
	  (CP_TYPE_RESTRICT_P (el)
	   || CP_TYPE_VOLATILE_P (el)
	   || CP_TYPE_CONST_P (el)))
	fprintf (stderr, "CV-");
      fprintf (stderr, "%s (%s at %p)\n",
	       name, get_tree_code_name (TREE_CODE (el)), (void *) el);
    }
}

/* Both decls and types can be substitution candidates, but sometimes
   they refer to the same thing.  For instance, a TYPE_DECL and
   RECORD_TYPE for the same class refer to the same thing, and should
   be treated accordingly in substitutions.  This function returns a
   canonicalized tree node representing NODE that is used when adding
   and substitution candidates and finding matches.  */

static inline tree
canonicalize_for_substitution (tree node)
{
  /* For a TYPE_DECL, use the type instead.  */
  if (TREE_CODE (node) == TYPE_DECL)
    node = TREE_TYPE (node);
  if (TYPE_P (node)
      && TYPE_CANONICAL (node) != node
      && TYPE_MAIN_VARIANT (node) != node)
    {
      tree orig = node;
      /* Here we want to strip the topmost typedef only.
         We need to do that so is_std_substitution can do proper
         name matching.  */
      if (TREE_CODE (node) == FUNCTION_TYPE)
	/* Use build_qualified_type and TYPE_QUALS here to preserve
	   the old buggy mangling of attribute noreturn with abi<5.  */
	node = build_qualified_type (TYPE_MAIN_VARIANT (node),
				     TYPE_QUALS (node));
      else
	node = cp_build_qualified_type (TYPE_MAIN_VARIANT (node),
					cp_type_quals (node));
      if (TREE_CODE (node) == FUNCTION_TYPE
	  || TREE_CODE (node) == METHOD_TYPE)
	node = build_ref_qualified_type (node, type_memfn_rqual (orig));
    }
  return node;
}

/* Add NODE as a substitution candidate.  NODE must not already be on
   the list of candidates.  */

static void
add_substitution (tree node)
{
  tree c;

  if (DEBUG_MANGLE)
    fprintf (stderr, "  ++ add_substitution (%s at %10p)\n",
	     get_tree_code_name (TREE_CODE (node)), (void *) node);

  /* Get the canonicalized substitution candidate for NODE.  */
  c = canonicalize_for_substitution (node);
  if (DEBUG_MANGLE && c != node)
    fprintf (stderr, "  ++ using candidate (%s at %10p)\n",
	     get_tree_code_name (TREE_CODE (node)), (void *) node);
  node = c;

#if ENABLE_CHECKING
  /* Make sure NODE isn't already a candidate.  */
  {
    int i;
    tree candidate;

    FOR_EACH_VEC_SAFE_ELT (G.substitutions, i, candidate)
      {
	gcc_assert (!(DECL_P (node) && node == candidate));
	gcc_assert (!(TYPE_P (node) && TYPE_P (candidate)
		      && same_type_p (node, candidate)));
      }
  }
#endif /* ENABLE_CHECKING */

  /* Put the decl onto the varray of substitution candidates.  */
  vec_safe_push (G.substitutions, node);

  if (DEBUG_MANGLE)
    dump_substitution_candidates ();
}

/* Helper function for find_substitution.  Returns nonzero if NODE,
   which may be a decl or a CLASS_TYPE, is a template-id with template
   name of substitution_index[INDEX] in the ::std namespace.  */

static inline int
is_std_substitution (const tree node,
		     const substitution_identifier_index_t index)
{
  tree type = NULL;
  tree decl = NULL;

  if (DECL_P (node))
    {
      type = TREE_TYPE (node);
      decl = node;
    }
  else if (CLASS_TYPE_P (node))
    {
      type = node;
      decl = TYPE_NAME (node);
    }
  else
    /* These are not the droids you're looking for.  */
    return 0;

  return (DECL_NAMESPACE_STD_P (CP_DECL_CONTEXT (decl))
	  && TYPE_LANG_SPECIFIC (type)
	  && TYPE_TEMPLATE_INFO (type)
	  && (DECL_NAME (TYPE_TI_TEMPLATE (type))
	      == subst_identifiers[index]));
}

/* Helper function for find_substitution.  Returns nonzero if NODE,
   which may be a decl or a CLASS_TYPE, is the template-id
   ::std::identifier<char>, where identifier is
   substitution_index[INDEX].  */

static inline int
is_std_substitution_char (const tree node,
			  const substitution_identifier_index_t index)
{
  tree args;
  /* Check NODE's name is ::std::identifier.  */
  if (!is_std_substitution (node, index))
    return 0;
  /* Figure out its template args.  */
  if (DECL_P (node))
    args = DECL_TI_ARGS (node);
  else if (CLASS_TYPE_P (node))
    args = CLASSTYPE_TI_ARGS (node);
  else
    /* Oops, not a template.  */
    return 0;
  /* NODE's template arg list should be <char>.  */
  return
    TREE_VEC_LENGTH (args) == 1
    && TREE_VEC_ELT (args, 0) == char_type_node;
}

/* Check whether a substitution should be used to represent NODE in
   the mangling.

   First, check standard special-case substitutions.

     <substitution> ::= St
	 # ::std

		    ::= Sa
	 # ::std::allocator

		    ::= Sb
	 # ::std::basic_string

		    ::= Ss
	 # ::std::basic_string<char,
			       ::std::char_traits<char>,
			       ::std::allocator<char> >

		    ::= Si
	 # ::std::basic_istream<char, ::std::char_traits<char> >

		    ::= So
	 # ::std::basic_ostream<char, ::std::char_traits<char> >

		    ::= Sd
	 # ::std::basic_iostream<char, ::std::char_traits<char> >

   Then examine the stack of currently available substitution
   candidates for entities appearing earlier in the same mangling

   If a substitution is found, write its mangled representation and
   return nonzero.  If none is found, just return zero.  */

static int
find_substitution (tree node)
{
  int i;
  const int size = vec_safe_length (G.substitutions);
  tree decl;
  tree type;

  if (DEBUG_MANGLE)
    fprintf (stderr, "  ++ find_substitution (%s at %p)\n",
	     get_tree_code_name (TREE_CODE (node)), (void *) node);

  /* Obtain the canonicalized substitution representation for NODE.
     This is what we'll compare against.  */
  node = canonicalize_for_substitution (node);

  /* Check for builtin substitutions.  */

  decl = TYPE_P (node) ? TYPE_NAME (node) : node;
  type = TYPE_P (node) ? node : TREE_TYPE (node);

  /* Check for std::allocator.  */
  if (decl
      && is_std_substitution (decl, SUBID_ALLOCATOR)
      && !CLASSTYPE_USE_TEMPLATE (TREE_TYPE (decl)))
    {
      write_string ("Sa");
      return 1;
    }

  /* Check for std::basic_string.  */
  if (decl && is_std_substitution (decl, SUBID_BASIC_STRING))
    {
      if (TYPE_P (node))
	{
	  /* If this is a type (i.e. a fully-qualified template-id),
	     check for
		 std::basic_string <char,
				    std::char_traits<char>,
				    std::allocator<char> > .  */
	  if (cp_type_quals (type) == TYPE_UNQUALIFIED
	      && CLASSTYPE_USE_TEMPLATE (type))
	    {
	      tree args = CLASSTYPE_TI_ARGS (type);
	      if (TREE_VEC_LENGTH (args) == 3
		  && same_type_p (TREE_VEC_ELT (args, 0), char_type_node)
		  && is_std_substitution_char (TREE_VEC_ELT (args, 1),
					       SUBID_CHAR_TRAITS)
		  && is_std_substitution_char (TREE_VEC_ELT (args, 2),
					       SUBID_ALLOCATOR))
		{
		  write_string ("Ss");
		  return 1;
		}
	    }
	}
      else
	/* Substitute for the template name only if this isn't a type.  */
	{
	  write_string ("Sb");
	  return 1;
	}
    }

  /* Check for basic_{i,o,io}stream.  */
  if (TYPE_P (node)
      && cp_type_quals (type) == TYPE_UNQUALIFIED
      && CLASS_TYPE_P (type)
      && CLASSTYPE_USE_TEMPLATE (type)
      && CLASSTYPE_TEMPLATE_INFO (type) != NULL)
    {
      /* First, check for the template
	 args <char, std::char_traits<char> > .  */
      tree args = CLASSTYPE_TI_ARGS (type);
      if (TREE_VEC_LENGTH (args) == 2
	  && TYPE_P (TREE_VEC_ELT (args, 0))
	  && same_type_p (TREE_VEC_ELT (args, 0), char_type_node)
	  && is_std_substitution_char (TREE_VEC_ELT (args, 1),
				       SUBID_CHAR_TRAITS))
	{
	  /* Got them.  Is this basic_istream?  */
	  if (is_std_substitution (decl, SUBID_BASIC_ISTREAM))
	    {
	      write_string ("Si");
	      return 1;
	    }
	  /* Or basic_ostream?  */
	  else if (is_std_substitution (decl, SUBID_BASIC_OSTREAM))
	    {
	      write_string ("So");
	      return 1;
	    }
	  /* Or basic_iostream?  */
	  else if (is_std_substitution (decl, SUBID_BASIC_IOSTREAM))
	    {
	      write_string ("Sd");
	      return 1;
	    }
	}
    }

  /* Check for namespace std.  */
  if (decl && DECL_NAMESPACE_STD_P (decl))
    {
      write_string ("St");
      return 1;
    }

  /* Now check the list of available substitutions for this mangling
     operation.  */
  for (i = 0; i < size; ++i)
    {
      tree candidate = (*G.substitutions)[i];
      /* NODE is a matched to a candidate if it's the same decl node or
	 if it's the same type.  */
      if (decl == candidate
	  || (TYPE_P (candidate) && type && TYPE_P (node)
	      && same_type_p (type, candidate))
	  || NESTED_TEMPLATE_MATCH (node, candidate))
	{
	  write_substitution (i);
	  return 1;
	}
    }

  /* No substitution found.  */
  return 0;
}


/* TOP_LEVEL is true, if this is being called at outermost level of
  mangling. It should be false when mangling a decl appearing in an
  expression within some other mangling.

  <mangled-name>      ::= _Z <encoding>  */

static void
write_mangled_name (const tree decl, bool top_level)
{
  MANGLE_TRACE_TREE ("mangled-name", decl);

  if (/* The names of `extern "C"' functions are not mangled.  */
      DECL_EXTERN_C_FUNCTION_P (decl)
      /* But overloaded operator names *are* mangled.  */
      && !DECL_OVERLOADED_OPERATOR_P (decl))
    {
    unmangled_name:;

      if (top_level)
	write_string (IDENTIFIER_POINTER (DECL_NAME (decl)));
      else
	{
	  /* The standard notes: "The <encoding> of an extern "C"
	     function is treated like global-scope data, i.e. as its
	     <source-name> without a type."  We cannot write
	     overloaded operators that way though, because it contains
	     characters invalid in assembler.  */
	  if (abi_version_at_least (2))
	    write_string ("_Z");
	  else
	    G.need_abi_warning = true;
	  write_source_name (DECL_NAME (decl));
	}
    }
  else if (VAR_P (decl)
	   /* The names of non-static global variables aren't mangled.  */
	   && DECL_EXTERNAL_LINKAGE_P (decl)
	   && (CP_DECL_CONTEXT (decl) == global_namespace
	       /* And neither are `extern "C"' variables.  */
	       || DECL_EXTERN_C_P (decl)))
    {
      if (top_level || abi_version_at_least (2))
	goto unmangled_name;
      else
	{
	  G.need_abi_warning = true;
	  goto mangled_name;
	}
    }
  else
    {
    mangled_name:;
      write_string ("_Z");
      write_encoding (decl);
    }
}

/*   <encoding>		::= <function name> <bare-function-type>
			::= <data name>  */

static void
write_encoding (const tree decl)
{
  MANGLE_TRACE_TREE ("encoding", decl);

  if (DECL_LANG_SPECIFIC (decl) && DECL_EXTERN_C_FUNCTION_P (decl))
    {
      /* For overloaded operators write just the mangled name
	 without arguments.  */
      if (DECL_OVERLOADED_OPERATOR_P (decl))
	write_name (decl, /*ignore_local_scope=*/0);
      else
	write_source_name (DECL_NAME (decl));
      return;
    }

  write_name (decl, /*ignore_local_scope=*/0);
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      tree fn_type;
      tree d;

      if (decl_is_template_id (decl, NULL))
	{
	  fn_type = get_mostly_instantiated_function_type (decl);
	  /* FN_TYPE will not have parameter types for in-charge or
	     VTT parameters.  Therefore, we pass NULL_TREE to
	     write_bare_function_type -- otherwise, it will get
	     confused about which artificial parameters to skip.  */
	  d = NULL_TREE;
	}
      else
	{
	  fn_type = TREE_TYPE (decl);
	  d = decl;
	}

      write_bare_function_type (fn_type,
				(!DECL_CONSTRUCTOR_P (decl)
				 && !DECL_DESTRUCTOR_P (decl)
				 && !DECL_CONV_FN_P (decl)
				 && decl_is_template_id (decl, NULL)),
				d);
    }
}

/* Lambdas can have a bit more context for mangling, specifically VAR_DECL
   or PARM_DECL context, which doesn't belong in DECL_CONTEXT.  */

static tree
decl_mangling_context (tree decl)
{
  tree tcontext = targetm.cxx.decl_mangling_context (decl);

  if (tcontext != NULL_TREE)
    return tcontext;

  if (TREE_CODE (decl) == TYPE_DECL
      && LAMBDA_TYPE_P (TREE_TYPE (decl)))
    {
      tree extra = LAMBDA_TYPE_EXTRA_SCOPE (TREE_TYPE (decl));
      if (extra)
	return extra;
    }
    else if (TREE_CODE (decl) == TYPE_DECL
	     && TREE_CODE (TREE_TYPE (decl)) == TEMPLATE_TYPE_PARM)
     /* template type parms have no mangling context.  */
      return NULL_TREE;
  return CP_DECL_CONTEXT (decl);
}

/* <name> ::= <unscoped-name>
	  ::= <unscoped-template-name> <template-args>
	  ::= <nested-name>
	  ::= <local-name>

   If IGNORE_LOCAL_SCOPE is nonzero, this production of <name> is
   called from <local-name>, which mangles the enclosing scope
   elsewhere and then uses this function to mangle just the part
   underneath the function scope.  So don't use the <local-name>
   production, to avoid an infinite recursion.  */

static void
write_name (tree decl, const int ignore_local_scope)
{
  tree context;

  MANGLE_TRACE_TREE ("name", decl);

  if (TREE_CODE (decl) == TYPE_DECL)
    {
      /* In case this is a typedef, fish out the corresponding
	 TYPE_DECL for the main variant.  */
      decl = TYPE_NAME (TYPE_MAIN_VARIANT (TREE_TYPE (decl)));
    }

  context = decl_mangling_context (decl);

  gcc_assert (context != NULL_TREE);

  /* A decl in :: or ::std scope is treated specially.  The former is
     mangled using <unscoped-name> or <unscoped-template-name>, the
     latter with a special substitution.  Also, a name that is
     directly in a local function scope is also mangled with
     <unscoped-name> rather than a full <nested-name>.  */
  if (context == global_namespace
      || DECL_NAMESPACE_STD_P (context)
      || (ignore_local_scope
	  && (TREE_CODE (context) == FUNCTION_DECL
	      || (abi_version_at_least (7)
		  && TREE_CODE (context) == PARM_DECL))))
    {
      tree template_info;
      /* Is this a template instance?  */
      if (decl_is_template_id (decl, &template_info))
	{
	  /* Yes: use <unscoped-template-name>.  */
	  write_unscoped_template_name (TI_TEMPLATE (template_info));
	  write_template_args (TI_ARGS (template_info));
	}
      else
	/* Everything else gets an <unqualified-name>.  */
	write_unscoped_name (decl);
    }
  else
    {
      /* Handle local names, unless we asked not to (that is, invoked
	 under <local-name>, to handle only the part of the name under
	 the local scope).  */
      if (!ignore_local_scope)
	{
	  /* Scan up the list of scope context, looking for a
	     function.  If we find one, this entity is in local
	     function scope.  local_entity tracks context one scope
	     level down, so it will contain the element that's
	     directly in that function's scope, either decl or one of
	     its enclosing scopes.  */
	  tree local_entity = decl;
	  while (context != global_namespace)
	    {
	      /* Make sure we're always dealing with decls.  */
	      if (TYPE_P (context))
		context = TYPE_NAME (context);
	      /* Is this a function?  */
	      if (TREE_CODE (context) == FUNCTION_DECL
		  || TREE_CODE (context) == PARM_DECL)
		{
		  /* Yes, we have local scope.  Use the <local-name>
		     production for the innermost function scope.  */
		  write_local_name (context, local_entity, decl);
		  return;
		}
	      /* Up one scope level.  */
	      local_entity = context;
	      context = decl_mangling_context (context);
	    }

	  /* No local scope found?  Fall through to <nested-name>.  */
	}

      /* Other decls get a <nested-name> to encode their scope.  */
      write_nested_name (decl);
    }
}

/* <unscoped-name> ::= <unqualified-name>
		   ::= St <unqualified-name>   # ::std::  */

static void
write_unscoped_name (const tree decl)
{
  tree context = decl_mangling_context (decl);

  MANGLE_TRACE_TREE ("unscoped-name", decl);

  /* Is DECL in ::std?  */
  if (DECL_NAMESPACE_STD_P (context))
    {
      write_string ("St");
      write_unqualified_name (decl);
    }
  else
    {
      /* If not, it should be either in the global namespace, or directly
	 in a local function scope.  A lambda can also be mangled in the
	 scope of a default argument.  */
      gcc_assert (context == global_namespace
		  || TREE_CODE (context) == PARM_DECL
		  || TREE_CODE (context) == FUNCTION_DECL);

      write_unqualified_name (decl);
    }
}

/* <unscoped-template-name> ::= <unscoped-name>
			    ::= <substitution>  */

static void
write_unscoped_template_name (const tree decl)
{
  MANGLE_TRACE_TREE ("unscoped-template-name", decl);

  if (find_substitution (decl))
    return;
  write_unscoped_name (decl);
  add_substitution (decl);
}

/* Write the nested name, including CV-qualifiers, of DECL.

   <nested-name> ::= N [<CV-qualifiers>] [<ref-qualifier>] <prefix> <unqualified-name> E
		 ::= N [<CV-qualifiers>] [<ref-qualifier>] <template-prefix> <template-args> E

   <ref-qualifier> ::= R # & ref-qualifier
                   ::= O # && ref-qualifier
   <CV-qualifiers> ::= [r] [V] [K]  */

static void
write_nested_name (const tree decl)
{
  tree template_info;

  MANGLE_TRACE_TREE ("nested-name", decl);

  write_char ('N');

  /* Write CV-qualifiers, if this is a member function.  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_NONSTATIC_MEMBER_FUNCTION_P (decl))
    {
      if (DECL_VOLATILE_MEMFUNC_P (decl))
	write_char ('V');
      if (DECL_CONST_MEMFUNC_P (decl))
	write_char ('K');
      if (FUNCTION_REF_QUALIFIED (TREE_TYPE (decl)))
	{
	  if (FUNCTION_RVALUE_QUALIFIED (TREE_TYPE (decl)))
	    write_char ('O');
	  else
	    write_char ('R');
	}
    }

  /* Is this a template instance?  */
  if (decl_is_template_id (decl, &template_info))
    {
      /* Yes, use <template-prefix>.  */
      write_template_prefix (decl);
      write_template_args (TI_ARGS (template_info));
    }
  else if (TREE_CODE (TREE_TYPE (decl)) == TYPENAME_TYPE)
    {
      tree name = TYPENAME_TYPE_FULLNAME (TREE_TYPE (decl));
      if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
	{
	  write_template_prefix (decl);
	  write_template_args (TREE_OPERAND (name, 1));
	}
      else
	{
	  write_prefix (decl_mangling_context (decl));
	  write_unqualified_name (decl);
	}
    }
  else
    {
      /* No, just use <prefix>  */
      write_prefix (decl_mangling_context (decl));
      write_unqualified_name (decl);
    }
  write_char ('E');
}

/* <prefix> ::= <prefix> <unqualified-name>
	    ::= <template-param>
	    ::= <template-prefix> <template-args>
	    ::= <decltype>
	    ::= # empty
	    ::= <substitution>  */

static void
write_prefix (const tree node)
{
  tree decl;
  /* Non-NULL if NODE represents a template-id.  */
  tree template_info = NULL;

  if (node == NULL
      || node == global_namespace)
    return;

  MANGLE_TRACE_TREE ("prefix", node);

  if (TREE_CODE (node) == DECLTYPE_TYPE)
    {
      write_type (node);
      return;
    }

  if (find_substitution (node))
    return;

  if (DECL_P (node))
    {
      /* If this is a function or parm decl, that means we've hit function
	 scope, so this prefix must be for a local name.  In this
	 case, we're under the <local-name> production, which encodes
	 the enclosing function scope elsewhere.  So don't continue
	 here.  */
      if (TREE_CODE (node) == FUNCTION_DECL
	  || TREE_CODE (node) == PARM_DECL)
	return;

      decl = node;
      decl_is_template_id (decl, &template_info);
    }
  else
    {
      /* Node is a type.  */
      decl = TYPE_NAME (node);
      if (CLASSTYPE_TEMPLATE_ID_P (node))
	template_info = TYPE_TEMPLATE_INFO (node);
    }

  /* In G++ 3.2, the name of the template parameter was used.  */
  if (TREE_CODE (node) == TEMPLATE_TYPE_PARM
      && !abi_version_at_least (2))
    G.need_abi_warning = true;

  if (TREE_CODE (node) == TEMPLATE_TYPE_PARM
      && abi_version_at_least (2))
    write_template_param (node);
  else if (template_info != NULL)
    /* Templated.  */
    {
      write_template_prefix (decl);
      write_template_args (TI_ARGS (template_info));
    }
  else if (TREE_CODE (TREE_TYPE (decl)) == TYPENAME_TYPE)
    {
      tree name = TYPENAME_TYPE_FULLNAME (TREE_TYPE (decl));
      if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
	{
	  write_template_prefix (decl);
	  write_template_args (TREE_OPERAND (name, 1));
	}
      else
	{
	  write_prefix (decl_mangling_context (decl));
	  write_unqualified_name (decl);
	}
    }
  else
    /* Not templated.  */
    {
      write_prefix (decl_mangling_context (decl));
      write_unqualified_name (decl);
      if (VAR_P (decl)
	  || TREE_CODE (decl) == FIELD_DECL)
	{
	  /* <data-member-prefix> := <member source-name> M */
	  write_char ('M');
	  return;
	}
    }

  add_substitution (node);
}

/* <template-prefix> ::= <prefix> <template component>
		     ::= <template-param>
		     ::= <substitution>  */

static void
write_template_prefix (const tree node)
{
  tree decl = DECL_P (node) ? node : TYPE_NAME (node);
  tree type = DECL_P (node) ? TREE_TYPE (node) : node;
  tree context = decl_mangling_context (decl);
  tree template_info;
  tree templ;
  tree substitution;

  MANGLE_TRACE_TREE ("template-prefix", node);

  /* Find the template decl.  */
  if (decl_is_template_id (decl, &template_info))
    templ = TI_TEMPLATE (template_info);
  else if (TREE_CODE (type) == TYPENAME_TYPE)
    /* For a typename type, all we have is the name.  */
    templ = DECL_NAME (decl);
  else
    {
      gcc_assert (CLASSTYPE_TEMPLATE_ID_P (type));

      templ = TYPE_TI_TEMPLATE (type);
    }

  /* For a member template, though, the template name for the
     innermost name must have all the outer template levels
     instantiated.  For instance, consider

       template<typename T> struct Outer {
	 template<typename U> struct Inner {};
       };

     The template name for `Inner' in `Outer<int>::Inner<float>' is
     `Outer<int>::Inner<U>'.  In g++, we don't instantiate the template
     levels separately, so there's no TEMPLATE_DECL available for this
     (there's only `Outer<T>::Inner<U>').

     In order to get the substitutions right, we create a special
     TREE_LIST to represent the substitution candidate for a nested
     template.  The TREE_PURPOSE is the template's context, fully
     instantiated, and the TREE_VALUE is the TEMPLATE_DECL for the inner
     template.

     So, for the example above, `Outer<int>::Inner' is represented as a
     substitution candidate by a TREE_LIST whose purpose is `Outer<int>'
     and whose value is `Outer<T>::Inner<U>'.  */
  if (TYPE_P (context))
    substitution = build_tree_list (context, templ);
  else
    substitution = templ;

  if (find_substitution (substitution))
    return;

  /* In G++ 3.2, the name of the template template parameter was used.  */
  if (TREE_TYPE (templ)
      && TREE_CODE (TREE_TYPE (templ)) == TEMPLATE_TEMPLATE_PARM
      && !abi_version_at_least (2))
    G.need_abi_warning = true;

  if (TREE_TYPE (templ)
      && TREE_CODE (TREE_TYPE (templ)) == TEMPLATE_TEMPLATE_PARM
      && abi_version_at_least (2))
    write_template_param (TREE_TYPE (templ));
  else
    {
      write_prefix (context);
      write_unqualified_name (decl);
    }

  add_substitution (substitution);
}

/* We don't need to handle thunks, vtables, or VTTs here.  Those are
   mangled through special entry points.

    <unqualified-name>  ::= <operator-name>
			::= <special-name>
			::= <source-name>
			::= <unnamed-type-name>
			::= <local-source-name> 

    <local-source-name>	::= L <source-name> <discriminator> */

static void
write_unqualified_id (tree identifier)
{
  if (IDENTIFIER_TYPENAME_P (identifier))
    write_conversion_operator_name (TREE_TYPE (identifier));
  else if (IDENTIFIER_OPNAME_P (identifier))
    {
      int i;
      const char *mangled_name = NULL;

      /* Unfortunately, there is no easy way to go from the
	 name of the operator back to the corresponding tree
	 code.  */
      for (i = 0; i < MAX_TREE_CODES; ++i)
	if (operator_name_info[i].identifier == identifier)
	  {
	    /* The ABI says that we prefer binary operator
	       names to unary operator names.  */
	    if (operator_name_info[i].arity == 2)
	      {
		mangled_name = operator_name_info[i].mangled_name;
		break;
	      }
	    else if (!mangled_name)
	      mangled_name = operator_name_info[i].mangled_name;
	  }
	else if (assignment_operator_name_info[i].identifier
		 == identifier)
	  {
	    mangled_name
	      = assignment_operator_name_info[i].mangled_name;
	    break;
	  }
      write_string (mangled_name);
    }
  else if (UDLIT_OPER_P (identifier))
    write_literal_operator_name (identifier);
  else
    write_source_name (identifier);
}

static void
write_unqualified_name (tree decl)
{
  MANGLE_TRACE_TREE ("unqualified-name", decl);

  if (identifier_p (decl))
    {
      write_unqualified_id (decl);
      return;
    }

  bool found = false;

  if (DECL_NAME (decl) == NULL_TREE)
    {
      found = true;
      gcc_assert (DECL_ASSEMBLER_NAME_SET_P (decl));
      write_source_name (DECL_ASSEMBLER_NAME (decl));
    }
  else if (DECL_DECLARES_FUNCTION_P (decl))
    {
      found = true;
      if (DECL_CONSTRUCTOR_P (decl))
	write_special_name_constructor (decl);
      else if (DECL_DESTRUCTOR_P (decl))
	write_special_name_destructor (decl);
      else if (DECL_CONV_FN_P (decl))
	{
	  /* Conversion operator. Handle it right here.
	     <operator> ::= cv <type>  */
	  tree type;
	  if (decl_is_template_id (decl, NULL))
	    {
	      tree fn_type;
	      fn_type = get_mostly_instantiated_function_type (decl);
	      type = TREE_TYPE (fn_type);
	    }
	  else if (FNDECL_USED_AUTO (decl))
	    type = (DECL_STRUCT_FUNCTION (decl)->language
		    ->x_auto_return_pattern);
	  else
	    type = DECL_CONV_FN_TYPE (decl);
	  write_conversion_operator_name (type);
	}
      else if (DECL_OVERLOADED_OPERATOR_P (decl))
	{
	  operator_name_info_t *oni;
	  if (DECL_ASSIGNMENT_OPERATOR_P (decl))
	    oni = assignment_operator_name_info;
	  else
	    oni = operator_name_info;

	  write_string (oni[DECL_OVERLOADED_OPERATOR_P (decl)].mangled_name);
	}
      else if (UDLIT_OPER_P (DECL_NAME (decl)))
	write_literal_operator_name (DECL_NAME (decl));
      else
	found = false;
    }

  if (found)
    /* OK */;
  else if (VAR_OR_FUNCTION_DECL_P (decl) && ! TREE_PUBLIC (decl)
	   && DECL_NAMESPACE_SCOPE_P (decl)
	   && decl_linkage (decl) == lk_internal)
    {
      MANGLE_TRACE_TREE ("local-source-name", decl);
      write_char ('L');
      write_source_name (DECL_NAME (decl));
      /* The default discriminator is 1, and that's all we ever use,
	 so there's no code to output one here.  */
    }
  else
    {
      tree type = TREE_TYPE (decl);

      if (TREE_CODE (decl) == TYPE_DECL
          && TYPE_ANONYMOUS_P (type))
        write_unnamed_type_name (type);
      else if (TREE_CODE (decl) == TYPE_DECL
               && LAMBDA_TYPE_P (type))
        write_closure_type_name (type);
      else
        write_source_name (DECL_NAME (decl));
    }

  /* We use the ABI tags from the primary template, ignoring tags on any
     specializations.  This is necessary because C++ doesn't require a
     specialization to be declared before it is used unless the use
     requires a complete type, but we need to get the tags right on
     incomplete types as well.  */
  if (tree tmpl = most_general_template (decl))
    decl = DECL_TEMPLATE_RESULT (tmpl);
  /* Don't crash on an unbound class template.  */
  if (decl)
    {
      tree attrs = (TREE_CODE (decl) == TYPE_DECL
		    ? TYPE_ATTRIBUTES (TREE_TYPE (decl))
		    : DECL_ATTRIBUTES (decl));
      write_abi_tags (lookup_attribute ("abi_tag", attrs));
    }
}

/* Write the unqualified-name for a conversion operator to TYPE.  */

static void
write_conversion_operator_name (const tree type)
{
  write_string ("cv");
  write_type (type);
}

/* Non-terminal <source-name>.  IDENTIFIER is an IDENTIFIER_NODE.

     <source-name> ::= </length/ number> <identifier>  */

static void
write_source_name (tree identifier)
{
  MANGLE_TRACE_TREE ("source-name", identifier);

  /* Never write the whole template-id name including the template
     arguments; we only want the template name.  */
  if (IDENTIFIER_TEMPLATE (identifier))
    identifier = IDENTIFIER_TEMPLATE (identifier);

  write_unsigned_number (IDENTIFIER_LENGTH (identifier));
  write_identifier (IDENTIFIER_POINTER (identifier));
}

/* Compare two TREE_STRINGs like strcmp.  */

int
tree_string_cmp (const void *p1, const void *p2)
{
  if (p1 == p2)
    return 0;
  tree s1 = *(const tree*)p1;
  tree s2 = *(const tree*)p2;
  return strcmp (TREE_STRING_POINTER (s1),
		 TREE_STRING_POINTER (s2));
}

/* ID is the name of a function or type with abi_tags attribute TAGS.
   Write out the name, suitably decorated.  */

static void
write_abi_tags (tree tags)
{
  if (tags == NULL_TREE)
    return;

  tags = TREE_VALUE (tags);

  vec<tree, va_gc> * vec = make_tree_vector();

  for (tree t = tags; t; t = TREE_CHAIN (t))
    {
      if (ABI_TAG_IMPLICIT (t))
	continue;
      tree str = TREE_VALUE (t);
      vec_safe_push (vec, str);
    }

  vec->qsort (tree_string_cmp);

  unsigned i; tree str;
  FOR_EACH_VEC_ELT (*vec, i, str)
    {
      write_string ("B");
      write_unsigned_number (TREE_STRING_LENGTH (str) - 1);
      write_identifier (TREE_STRING_POINTER (str));
    }

  release_tree_vector (vec);
}

/* Write a user-defined literal operator.
          ::= li <source-name>    # "" <source-name>
   IDENTIFIER is an LITERAL_IDENTIFIER_NODE.  */

static void
write_literal_operator_name (tree identifier)
{
  const char* suffix = UDLIT_OP_SUFFIX (identifier);
  write_identifier (UDLIT_OP_MANGLED_PREFIX);
  write_unsigned_number (strlen (suffix));
  write_identifier (suffix);
}

/* Encode 0 as _, and 1+ as n-1_.  */

static void
write_compact_number (int num)
{
  if (num > 0)
    write_unsigned_number (num - 1);
  write_char ('_');
}

/* Return how many unnamed types precede TYPE in its enclosing class.  */

static int
nested_anon_class_index (tree type)
{
  int index = 0;
  tree member = TYPE_FIELDS (TYPE_CONTEXT (type));
  for (; member; member = DECL_CHAIN (member))
    if (DECL_IMPLICIT_TYPEDEF_P (member))
      {
	tree memtype = TREE_TYPE (member);
	if (memtype == type)
	  return index;
	else if (TYPE_ANONYMOUS_P (memtype))
	  ++index;
      }

  gcc_unreachable ();
}

/* <unnamed-type-name> ::= Ut [ <nonnegative number> ] _ */

static void
write_unnamed_type_name (const tree type)
{
  int discriminator;
  MANGLE_TRACE_TREE ("unnamed-type-name", type);

  if (TYPE_FUNCTION_SCOPE_P (type))
    discriminator = local_class_index (type);
  else if (TYPE_CLASS_SCOPE_P (type))
    discriminator = nested_anon_class_index (type);
  else
    {
      gcc_assert (no_linkage_check (type, /*relaxed_p=*/true));
      /* Just use the old mangling at namespace scope.  */
      write_source_name (TYPE_IDENTIFIER (type));
      return;
    }

  write_string ("Ut");
  write_compact_number (discriminator);
}

/* <closure-type-name> ::= Ul <lambda-sig> E [ <nonnegative number> ] _
   <lambda-sig> ::= <parameter type>+  # Parameter types or "v" if the lambda has no parameters */

static void
write_closure_type_name (const tree type)
{
  tree fn = lambda_function (type);
  tree lambda = CLASSTYPE_LAMBDA_EXPR (type);
  tree parms = TYPE_ARG_TYPES (TREE_TYPE (fn));

  MANGLE_TRACE_TREE ("closure-type-name", type);

  write_string ("Ul");
  write_method_parms (parms, /*method_p=*/1, fn);
  write_char ('E');
  write_compact_number (LAMBDA_EXPR_DISCRIMINATOR (lambda));
}

/* Convert NUMBER to ascii using base BASE and generating at least
   MIN_DIGITS characters. BUFFER points to the _end_ of the buffer
   into which to store the characters. Returns the number of
   characters generated (these will be laid out in advance of where
   BUFFER points).  */

static int
hwint_to_ascii (unsigned HOST_WIDE_INT number, const unsigned int base,
		char *buffer, const unsigned int min_digits)
{
  static const char base_digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  unsigned digits = 0;

  while (number)
    {
      unsigned HOST_WIDE_INT d = number / base;

      *--buffer = base_digits[number - d * base];
      digits++;
      number = d;
    }
  while (digits < min_digits)
    {
      *--buffer = base_digits[0];
      digits++;
    }
  return digits;
}

/* Non-terminal <number>.

     <number> ::= [n] </decimal integer/>  */

static void
write_number (unsigned HOST_WIDE_INT number, const int unsigned_p,
	      const unsigned int base)
{
  char buffer[sizeof (HOST_WIDE_INT) * 8];
  unsigned count = 0;

  if (!unsigned_p && (HOST_WIDE_INT) number < 0)
    {
      write_char ('n');
      number = -((HOST_WIDE_INT) number);
    }
  count = hwint_to_ascii (number, base, buffer + sizeof (buffer), 1);
  write_chars (buffer + sizeof (buffer) - count, count);
}

/* Write out an integral CST in decimal. Most numbers are small, and
   representable in a HOST_WIDE_INT. Occasionally we'll have numbers
   bigger than that, which we must deal with.  */

static inline void
write_integer_cst (const tree cst)
{
  int sign = tree_int_cst_sgn (cst);
  widest_int abs_value = wi::abs (wi::to_widest (cst));
  if (!wi::fits_uhwi_p (abs_value))
    {
      /* A bignum. We do this in chunks, each of which fits in a
	 HOST_WIDE_INT.  */
      char buffer[sizeof (HOST_WIDE_INT) * 8 * 2];
      unsigned HOST_WIDE_INT chunk;
      unsigned chunk_digits;
      char *ptr = buffer + sizeof (buffer);
      unsigned count = 0;
      tree n, base, type;
      int done;

      /* HOST_WIDE_INT must be at least 32 bits, so 10^9 is
	 representable.  */
      chunk = 1000000000;
      chunk_digits = 9;

      if (sizeof (HOST_WIDE_INT) >= 8)
	{
	  /* It is at least 64 bits, so 10^18 is representable.  */
	  chunk_digits = 18;
	  chunk *= chunk;
	}

      type = c_common_signed_or_unsigned_type (1, TREE_TYPE (cst));
      base = build_int_cstu (type, chunk);
      n = wide_int_to_tree (type, cst);

      if (sign < 0)
	{
	  write_char ('n');
	  n = fold_build1_loc (input_location, NEGATE_EXPR, type, n);
	}
      do
	{
	  tree d = fold_build2_loc (input_location, FLOOR_DIV_EXPR, type, n, base);
	  tree tmp = fold_build2_loc (input_location, MULT_EXPR, type, d, base);
	  unsigned c;

	  done = integer_zerop (d);
	  tmp = fold_build2_loc (input_location, MINUS_EXPR, type, n, tmp);
	  c = hwint_to_ascii (TREE_INT_CST_LOW (tmp), 10, ptr,
			      done ? 1 : chunk_digits);
	  ptr -= c;
	  count += c;
	  n = d;
	}
      while (!done);
      write_chars (ptr, count);
    }
  else
    {
      /* A small num.  */
      if (sign < 0)
	write_char ('n');
      write_unsigned_number (abs_value.to_uhwi ());
    }
}

/* Write out a floating-point literal.

    "Floating-point literals are encoded using the bit pattern of the
    target processor's internal representation of that number, as a
    fixed-length lowercase hexadecimal string, high-order bytes first
    (even if the target processor would store low-order bytes first).
    The "n" prefix is not used for floating-point literals; the sign
    bit is encoded with the rest of the number.

    Here are some examples, assuming the IEEE standard representation
    for floating point numbers.  (Spaces are for readability, not
    part of the encoding.)

	1.0f			Lf 3f80 0000 E
       -1.0f			Lf bf80 0000 E
	1.17549435e-38f		Lf 0080 0000 E
	1.40129846e-45f		Lf 0000 0001 E
	0.0f			Lf 0000 0000 E"

   Caller is responsible for the Lx and the E.  */
static void
write_real_cst (const tree value)
{
  if (abi_version_at_least (2))
    {
      long target_real[4];  /* largest supported float */
      char buffer[9];       /* eight hex digits in a 32-bit number */
      int i, limit, dir;

      tree type = TREE_TYPE (value);
      int words = GET_MODE_BITSIZE (TYPE_MODE (type)) / 32;

      real_to_target (target_real, &TREE_REAL_CST (value),
		      TYPE_MODE (type));

      /* The value in target_real is in the target word order,
	 so we must write it out backward if that happens to be
	 little-endian.  write_number cannot be used, it will
	 produce uppercase.  */
      if (FLOAT_WORDS_BIG_ENDIAN)
	i = 0, limit = words, dir = 1;
      else
	i = words - 1, limit = -1, dir = -1;

      for (; i != limit; i += dir)
	{
	  sprintf (buffer, "%08lx", (unsigned long) target_real[i]);
	  write_chars (buffer, 8);
	}
    }
  else
    {
      /* In G++ 3.3 and before the REAL_VALUE_TYPE was written out
	 literally.  Note that compatibility with 3.2 is impossible,
	 because the old floating-point emulator used a different
	 format for REAL_VALUE_TYPE.  */
      size_t i;
      for (i = 0; i < sizeof (TREE_REAL_CST (value)); ++i)
	write_number (((unsigned char *) &TREE_REAL_CST (value))[i],
		      /*unsigned_p*/ 1,
		      /*base*/ 16);
      G.need_abi_warning = 1;
    }
}

/* Non-terminal <identifier>.

     <identifier> ::= </unqualified source code identifier>  */

static void
write_identifier (const char *identifier)
{
  MANGLE_TRACE ("identifier", identifier);
  write_string (identifier);
}

/* Handle constructor productions of non-terminal <special-name>.
   CTOR is a constructor FUNCTION_DECL.

     <special-name> ::= C1   # complete object constructor
		    ::= C2   # base object constructor
		    ::= C3   # complete object allocating constructor

   Currently, allocating constructors are never used.  */

static void
write_special_name_constructor (const tree ctor)
{
  if (DECL_BASE_CONSTRUCTOR_P (ctor))
    write_string ("C2");
  /* This is the old-style "[unified]" constructor.
     In some cases, we may emit this function and call
     it from the clones in order to share code and save space.  */
  else if (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (ctor))
    write_string ("C4");
  else
    {
      gcc_assert (DECL_COMPLETE_CONSTRUCTOR_P (ctor));
      write_string ("C1");
    }
}

/* Handle destructor productions of non-terminal <special-name>.
   DTOR is a destructor FUNCTION_DECL.

     <special-name> ::= D0 # deleting (in-charge) destructor
		    ::= D1 # complete object (in-charge) destructor
		    ::= D2 # base object (not-in-charge) destructor  */

static void
write_special_name_destructor (const tree dtor)
{
  if (DECL_DELETING_DESTRUCTOR_P (dtor))
    write_string ("D0");
  else if (DECL_BASE_DESTRUCTOR_P (dtor))
    write_string ("D2");
  else if (DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (dtor))
    /* This is the old-style "[unified]" destructor.
       In some cases, we may emit this function and call
       it from the clones in order to share code and save space.  */
    write_string ("D4");
  else
    {
      gcc_assert (DECL_COMPLETE_DESTRUCTOR_P (dtor));
      write_string ("D1");
    }
}

/* Scan the vector of local classes and return how many others with the
   same name (or same no name) and context precede ENTITY.  */

static int
local_class_index (tree entity)
{
  int ix, discriminator = 0;
  tree name = (TYPE_ANONYMOUS_P (entity) ? NULL_TREE
	       : TYPE_IDENTIFIER (entity));
  tree ctx = TYPE_CONTEXT (entity);
  for (ix = 0; ; ix++)
    {
      tree type = (*local_classes)[ix];
      if (type == entity)
	return discriminator;
      if (TYPE_CONTEXT (type) == ctx
	  && (name ? TYPE_IDENTIFIER (type) == name
	      : TYPE_ANONYMOUS_P (type)))
	++discriminator;
    }
  gcc_unreachable ();
}

/* Return the discriminator for ENTITY appearing inside
   FUNCTION.  The discriminator is the lexical ordinal of VAR among
   entities with the same name in the same FUNCTION.  */

static int
discriminator_for_local_entity (tree entity)
{
  if (DECL_DISCRIMINATOR_P (entity))
    {
      if (DECL_DISCRIMINATOR_SET_P (entity))
	return DECL_DISCRIMINATOR (entity);
      else
	/* The first entity with a particular name doesn't get
	   DECL_DISCRIMINATOR set up.  */
	return 0;
    }
  else if (TREE_CODE (entity) == TYPE_DECL)
    {
      /* Scan the list of local classes.  */
      entity = TREE_TYPE (entity);

      /* Lambdas and unnamed types have their own discriminators.  */
      if (LAMBDA_TYPE_P (entity) || TYPE_ANONYMOUS_P (entity))
	return 0;

      return local_class_index (entity);
    }
  else
    gcc_unreachable ();
}

/* Return the discriminator for STRING, a string literal used inside
   FUNCTION.  The discriminator is the lexical ordinal of STRING among
   string literals used in FUNCTION.  */

static int
discriminator_for_string_literal (tree /*function*/,
				  tree /*string*/)
{
  /* For now, we don't discriminate amongst string literals.  */
  return 0;
}

/*   <discriminator> := _ <number>

   The discriminator is used only for the second and later occurrences
   of the same name within a single function. In this case <number> is
   n - 2, if this is the nth occurrence, in lexical order.  */

static void
write_discriminator (const int discriminator)
{
  /* If discriminator is zero, don't write anything.  Otherwise...  */
  if (discriminator > 0)
    {
      write_char ('_');
      write_unsigned_number (discriminator - 1);
    }
}

/* Mangle the name of a function-scope entity.  FUNCTION is the
   FUNCTION_DECL for the enclosing function, or a PARM_DECL for lambdas in
   default argument scope.  ENTITY is the decl for the entity itself.
   LOCAL_ENTITY is the entity that's directly scoped in FUNCTION_DECL,
   either ENTITY itself or an enclosing scope of ENTITY.

     <local-name> := Z <function encoding> E <entity name> [<discriminator>]
		  := Z <function encoding> E s [<discriminator>]
		  := Z <function encoding> Ed [ <parameter number> ] _ <entity name> */

static void
write_local_name (tree function, const tree local_entity,
		  const tree entity)
{
  tree parm = NULL_TREE;

  MANGLE_TRACE_TREE ("local-name", entity);

  if (TREE_CODE (function) == PARM_DECL)
    {
      parm = function;
      function = DECL_CONTEXT (parm);
    }

  write_char ('Z');
  write_encoding (function);
  write_char ('E');

  /* For this purpose, parameters are numbered from right-to-left.  */
  if (parm)
    {
      tree t;
      int i = 0;
      for (t = DECL_ARGUMENTS (function); t; t = DECL_CHAIN (t))
	{
	  if (t == parm)
	    i = 1;
	  else if (i)
	    ++i;
	}
      write_char ('d');
      write_compact_number (i - 1);
    }

  if (TREE_CODE (entity) == STRING_CST)
    {
      write_char ('s');
      write_discriminator (discriminator_for_string_literal (function,
							     entity));
    }
  else
    {
      /* Now the <entity name>.  Let write_name know its being called
	 from <local-name>, so it doesn't try to process the enclosing
	 function scope again.  */
      write_name (entity, /*ignore_local_scope=*/1);
      write_discriminator (discriminator_for_local_entity (local_entity));
    }
}

/* Non-terminals <type> and <CV-qualifier>.

     <type> ::= <builtin-type>
	    ::= <function-type>
	    ::= <class-enum-type>
	    ::= <array-type>
	    ::= <pointer-to-member-type>
	    ::= <template-param>
	    ::= <substitution>
	    ::= <CV-qualifier>
	    ::= P <type>    # pointer-to
	    ::= R <type>    # reference-to
	    ::= C <type>    # complex pair (C 2000)
	    ::= G <type>    # imaginary (C 2000)     [not supported]
	    ::= U <source-name> <type>   # vendor extended type qualifier

   C++0x extensions

     <type> ::= RR <type>   # rvalue reference-to
     <type> ::= Dt <expression> # decltype of an id-expression or 
                                # class member access
     <type> ::= DT <expression> # decltype of an expression
     <type> ::= Dn              # decltype of nullptr

   TYPE is a type node.  */

static void
write_type (tree type)
{
  /* This gets set to nonzero if TYPE turns out to be a (possibly
     CV-qualified) builtin type.  */
  int is_builtin_type = 0;

  MANGLE_TRACE_TREE ("type", type);

  if (type == error_mark_node)
    return;

  type = canonicalize_for_substitution (type);
  if (find_substitution (type))
    return;


  if (write_CV_qualifiers_for_type (type) > 0)
    /* If TYPE was CV-qualified, we just wrote the qualifiers; now
       mangle the unqualified type.  The recursive call is needed here
       since both the qualified and unqualified types are substitution
       candidates.  */
    {
      tree t = TYPE_MAIN_VARIANT (type);
      if (TREE_CODE (t) == FUNCTION_TYPE
	  || TREE_CODE (t) == METHOD_TYPE)
	{
	  t = build_ref_qualified_type (t, type_memfn_rqual (type));
	  if (abi_version_at_least (8))
	    /* Avoid adding the unqualified function type as a substitution.  */
	    write_function_type (t);
	  else
	    write_type (t);
	}
      else
	write_type (t);
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    /* It is important not to use the TYPE_MAIN_VARIANT of TYPE here
       so that the cv-qualification of the element type is available
       in write_array_type.  */
    write_array_type (type);
  else
    {
      tree type_orig = type;

      /* See through any typedefs.  */
      type = TYPE_MAIN_VARIANT (type);
      if (TREE_CODE (type) == FUNCTION_TYPE
	  || TREE_CODE (type) == METHOD_TYPE)
	type = build_ref_qualified_type (type, type_memfn_rqual (type_orig));

      /* According to the C++ ABI, some library classes are passed the
	 same as the scalar type of their single member and use the same
	 mangling.  */
      if (TREE_CODE (type) == RECORD_TYPE && TYPE_TRANSPARENT_AGGR (type))
	type = TREE_TYPE (first_field (type));

      if (TYPE_PTRDATAMEM_P (type))
	write_pointer_to_member_type (type);
      else
        {
	  /* Handle any target-specific fundamental types.  */
	  const char *target_mangling
	    = targetm.mangle_type (type_orig);

	  if (target_mangling)
	    {
	      write_string (target_mangling);
	      /* Add substitutions for types other than fundamental
		 types.  */
	      if (!VOID_TYPE_P (type)
		  && TREE_CODE (type) != INTEGER_TYPE
		  && TREE_CODE (type) != REAL_TYPE
		  && TREE_CODE (type) != BOOLEAN_TYPE)
		add_substitution (type);
	      return;
	    }

	  switch (TREE_CODE (type))
	    {
	    case VOID_TYPE:
	    case BOOLEAN_TYPE:
	    case INTEGER_TYPE:  /* Includes wchar_t.  */
	    case REAL_TYPE:
	    case FIXED_POINT_TYPE:
	      {
		/* If this is a typedef, TYPE may not be one of
		   the standard builtin type nodes, but an alias of one.  Use
		   TYPE_MAIN_VARIANT to get to the underlying builtin type.  */
		write_builtin_type (TYPE_MAIN_VARIANT (type));
		++is_builtin_type;
	      }
	      break;

	    case COMPLEX_TYPE:
	      write_char ('C');
	      write_type (TREE_TYPE (type));
	      break;

	    case FUNCTION_TYPE:
	    case METHOD_TYPE:
	      write_function_type (type);
	      break;

	    case UNION_TYPE:
	    case RECORD_TYPE:
	    case ENUMERAL_TYPE:
	      /* A pointer-to-member function is represented as a special
		 RECORD_TYPE, so check for this first.  */
	      if (TYPE_PTRMEMFUNC_P (type))
		write_pointer_to_member_type (type);
	      else
		write_class_enum_type (type);
	      break;

	    case TYPENAME_TYPE:
	    case UNBOUND_CLASS_TEMPLATE:
	      /* We handle TYPENAME_TYPEs and UNBOUND_CLASS_TEMPLATEs like
		 ordinary nested names.  */
	      write_nested_name (TYPE_STUB_DECL (type));
	      break;

	    case POINTER_TYPE:
	    case REFERENCE_TYPE:
	      if (TYPE_PTR_P (type))
		write_char ('P');
	      else if (TYPE_REF_IS_RVALUE (type))
		write_char ('O');
              else
                write_char ('R');
	      {
		tree target = TREE_TYPE (type);
		/* Attribute const/noreturn are not reflected in mangling.
		   We strip them here rather than at a lower level because
		   a typedef or template argument can have function type
		   with function-cv-quals (that use the same representation),
		   but you can't have a pointer/reference to such a type.  */
		if (abi_version_at_least (5)
		    && TREE_CODE (target) == FUNCTION_TYPE)
		  target = build_qualified_type (target, TYPE_UNQUALIFIED);
		write_type (target);
	      }
	      break;

	    case TEMPLATE_TYPE_PARM:
	      if (is_auto (type))
		{
		  if (AUTO_IS_DECLTYPE (type))
		    write_identifier ("Dc");
		  else
		    write_identifier ("Da");
		  ++is_builtin_type;
		  break;
		}
	      /* else fall through.  */
	    case TEMPLATE_PARM_INDEX:
	      write_template_param (type);
	      break;

	    case TEMPLATE_TEMPLATE_PARM:
	      write_template_template_param (type);
	      break;

	    case BOUND_TEMPLATE_TEMPLATE_PARM:
	      write_template_template_param (type);
	      write_template_args
		(TI_ARGS (TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (type)));
	      break;

	    case VECTOR_TYPE:
	      if (abi_version_at_least (4))
		{
		  write_string ("Dv");
		  /* Non-constant vector size would be encoded with
		     _ expression, but we don't support that yet.  */
		  write_unsigned_number (TYPE_VECTOR_SUBPARTS (type));
		  write_char ('_');
		}
	      else
		{
		  G.need_abi_warning = 1;
		  write_string ("U8__vector");
		}
	      write_type (TREE_TYPE (type));
	      break;

            case TYPE_PACK_EXPANSION:
              write_string ("Dp");
              write_type (PACK_EXPANSION_PATTERN (type));
              break;

            case DECLTYPE_TYPE:
	      /* These shouldn't make it into mangling.  */
	      gcc_assert (!DECLTYPE_FOR_LAMBDA_CAPTURE (type)
			  && !DECLTYPE_FOR_LAMBDA_PROXY (type));

	      /* In ABI <5, we stripped decltype of a plain decl.  */
	      if (!abi_version_at_least (5)
		  && DECLTYPE_TYPE_ID_EXPR_OR_MEMBER_ACCESS_P (type))
		{
		  tree expr = DECLTYPE_TYPE_EXPR (type);
		  tree etype = NULL_TREE;
		  switch (TREE_CODE (expr))
		    {
		    case VAR_DECL:
		    case PARM_DECL:
		    case RESULT_DECL:
		    case FUNCTION_DECL:
		    case CONST_DECL:
		    case TEMPLATE_PARM_INDEX:
		      etype = TREE_TYPE (expr);
		      break;

		    default:
		      break;
		    }

		  if (etype && !type_uses_auto (etype))
		    {
		      G.need_abi_warning = 1;
		      write_type (etype);
		      return;
		    }
		}

              write_char ('D');
              if (DECLTYPE_TYPE_ID_EXPR_OR_MEMBER_ACCESS_P (type))
                write_char ('t');
              else
                write_char ('T');
	      ++cp_unevaluated_operand;
              write_expression (DECLTYPE_TYPE_EXPR (type));
	      --cp_unevaluated_operand;
              write_char ('E');
              break;

	    case NULLPTR_TYPE:
	      write_string ("Dn");
	      if (abi_version_at_least (7))
		++is_builtin_type;
	      break;

	    case TYPEOF_TYPE:
	      sorry ("mangling typeof, use decltype instead");
	      break;

	    case UNDERLYING_TYPE:
	      sorry ("mangling __underlying_type");
	      break;

	    case LANG_TYPE:
	      /* fall through.  */

	    default:
	      gcc_unreachable ();
	    }
	}
    }

  /* Types other than builtin types are substitution candidates.  */
  if (!is_builtin_type)
    add_substitution (type);
}

/* Non-terminal <CV-qualifiers> for type nodes.  Returns the number of
   CV-qualifiers written for TYPE.

     <CV-qualifiers> ::= [r] [V] [K]  */

static int
write_CV_qualifiers_for_type (const tree type)
{
  int num_qualifiers = 0;

  /* The order is specified by:

       "In cases where multiple order-insensitive qualifiers are
       present, they should be ordered 'K' (closest to the base type),
       'V', 'r', and 'U' (farthest from the base type) ..."

     Note that we do not use cp_type_quals below; given "const
     int[3]", the "const" is emitted with the "int", not with the
     array.  */
  cp_cv_quals quals = TYPE_QUALS (type);

  if (quals & TYPE_QUAL_RESTRICT)
    {
      write_char ('r');
      ++num_qualifiers;
    }
  if (quals & TYPE_QUAL_VOLATILE)
    {
      write_char ('V');
      ++num_qualifiers;
    }
  if (quals & TYPE_QUAL_CONST)
    {
      write_char ('K');
      ++num_qualifiers;
    }

  return num_qualifiers;
}

/* Non-terminal <builtin-type>.

     <builtin-type> ::= v   # void
		    ::= b   # bool
		    ::= w   # wchar_t
		    ::= c   # char
		    ::= a   # signed char
		    ::= h   # unsigned char
		    ::= s   # short
		    ::= t   # unsigned short
		    ::= i   # int
		    ::= j   # unsigned int
		    ::= l   # long
		    ::= m   # unsigned long
		    ::= x   # long long, __int64
		    ::= y   # unsigned long long, __int64
		    ::= n   # __int128
		    ::= o   # unsigned __int128
		    ::= f   # float
		    ::= d   # double
		    ::= e   # long double, __float80
		    ::= g   # __float128          [not supported]
		    ::= u <source-name>  # vendor extended type */

static void
write_builtin_type (tree type)
{
  if (TYPE_CANONICAL (type))
    type = TYPE_CANONICAL (type);

  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
      write_char ('v');
      break;

    case BOOLEAN_TYPE:
      write_char ('b');
      break;

    case INTEGER_TYPE:
      /* TYPE may still be wchar_t, char16_t, or char32_t, since that
	 isn't in integer_type_nodes.  */
      if (type == wchar_type_node)
	write_char ('w');
      else if (type == char16_type_node)
	write_string ("Ds");
      else if (type == char32_type_node)
	write_string ("Di");
      else if (TYPE_FOR_JAVA (type))
	write_java_integer_type_codes (type);
      else
	{
	  size_t itk;
	  /* Assume TYPE is one of the shared integer type nodes.  Find
	     it in the array of these nodes.  */
	iagain:
	  for (itk = 0; itk < itk_none; ++itk)
	    if (integer_types[itk] != NULL_TREE
		&& type == integer_types[itk])
	      {
		/* Print the corresponding single-letter code.  */
		write_char (integer_type_codes[itk]);
		break;
	      }

	  if (itk == itk_none)
	    {
	      tree t = c_common_type_for_mode (TYPE_MODE (type),
					       TYPE_UNSIGNED (type));
	      if (type != t)
		{
		  type = t;
		  goto iagain;
		}

	      if (TYPE_PRECISION (type) == 128)
		write_char (TYPE_UNSIGNED (type) ? 'o' : 'n');
	      else
		{
		  /* Allow for cases where TYPE is not one of the shared
		     integer type nodes and write a "vendor extended builtin
		     type" with a name the form intN or uintN, respectively.
		     Situations like this can happen if you have an
		     __attribute__((__mode__(__SI__))) type and use exotic
		     switches like '-mint8' on AVR.  Of course, this is
		     undefined by the C++ ABI (and '-mint8' is not even
		     Standard C conforming), but when using such special
		     options you're pretty much in nowhere land anyway.  */
		  const char *prefix;
		  char prec[11];	/* up to ten digits for an unsigned */

		  prefix = TYPE_UNSIGNED (type) ? "uint" : "int";
		  sprintf (prec, "%u", (unsigned) TYPE_PRECISION (type));
		  write_char ('u');	/* "vendor extended builtin type" */
		  write_unsigned_number (strlen (prefix) + strlen (prec));
		  write_string (prefix);
		  write_string (prec);
		}
	    }
	}
      break;

    case REAL_TYPE:
      if (type == float_type_node
	  || type == java_float_type_node)
	write_char ('f');
      else if (type == double_type_node
	       || type == java_double_type_node)
	write_char ('d');
      else if (type == long_double_type_node)
	write_char ('e');
      else if (type == dfloat32_type_node)
	write_string ("Df");
      else if (type == dfloat64_type_node)
	write_string ("Dd");
      else if (type == dfloat128_type_node)
	write_string ("De");
      else
	gcc_unreachable ();
      break;

    case FIXED_POINT_TYPE:
      write_string ("DF");
      if (GET_MODE_IBIT (TYPE_MODE (type)) > 0)
	write_unsigned_number (GET_MODE_IBIT (TYPE_MODE (type)));
      if (type == fract_type_node
	  || type == sat_fract_type_node
	  || type == accum_type_node
	  || type == sat_accum_type_node)
	write_char ('i');
      else if (type == unsigned_fract_type_node
	       || type == sat_unsigned_fract_type_node
	       || type == unsigned_accum_type_node
	       || type == sat_unsigned_accum_type_node)
	write_char ('j');
      else if (type == short_fract_type_node
	       || type == sat_short_fract_type_node
	       || type == short_accum_type_node
	       || type == sat_short_accum_type_node)
	write_char ('s');
      else if (type == unsigned_short_fract_type_node
	       || type == sat_unsigned_short_fract_type_node
	       || type == unsigned_short_accum_type_node
	       || type == sat_unsigned_short_accum_type_node)
	write_char ('t');
      else if (type == long_fract_type_node
	       || type == sat_long_fract_type_node
	       || type == long_accum_type_node
	       || type == sat_long_accum_type_node)
	write_char ('l');
      else if (type == unsigned_long_fract_type_node
	       || type == sat_unsigned_long_fract_type_node
	       || type == unsigned_long_accum_type_node
	       || type == sat_unsigned_long_accum_type_node)
	write_char ('m');
      else if (type == long_long_fract_type_node
	       || type == sat_long_long_fract_type_node
	       || type == long_long_accum_type_node
	       || type == sat_long_long_accum_type_node)
	write_char ('x');
      else if (type == unsigned_long_long_fract_type_node
	       || type == sat_unsigned_long_long_fract_type_node
	       || type == unsigned_long_long_accum_type_node
	       || type == sat_unsigned_long_long_accum_type_node)
	write_char ('y');
      else
	sorry ("mangling unknown fixed point type");
      write_unsigned_number (GET_MODE_FBIT (TYPE_MODE (type)));
      if (TYPE_SATURATING (type))
	write_char ('s');
      else
	write_char ('n');
      break;

    default:
      gcc_unreachable ();
    }
}

/* Non-terminal <function-type>.  NODE is a FUNCTION_TYPE or
   METHOD_TYPE.  The return type is mangled before the parameter
   types.

     <function-type> ::= F [Y] <bare-function-type> [<ref-qualifier>] E   */

static void
write_function_type (const tree type)
{
  MANGLE_TRACE_TREE ("function-type", type);

  /* For a pointer to member function, the function type may have
     cv-qualifiers, indicating the quals for the artificial 'this'
     parameter.  */
  if (TREE_CODE (type) == METHOD_TYPE)
    {
      /* The first parameter must be a POINTER_TYPE pointing to the
	 `this' parameter.  */
      tree this_type = class_of_this_parm (type);
      write_CV_qualifiers_for_type (this_type);
    }

  write_char ('F');
  /* We don't track whether or not a type is `extern "C"'.  Note that
     you can have an `extern "C"' function that does not have
     `extern "C"' type, and vice versa:

       extern "C" typedef void function_t();
       function_t f; // f has C++ linkage, but its type is
		     // `extern "C"'

       typedef void function_t();
       extern "C" function_t f; // Vice versa.

     See [dcl.link].  */
  write_bare_function_type (type, /*include_return_type_p=*/1,
			    /*decl=*/NULL);
  if (FUNCTION_REF_QUALIFIED (type))
    {
      if (FUNCTION_RVALUE_QUALIFIED (type))
	write_char ('O');
      else
	write_char ('R');
    }
  write_char ('E');
}

/* Non-terminal <bare-function-type>.  TYPE is a FUNCTION_TYPE or
   METHOD_TYPE.  If INCLUDE_RETURN_TYPE is nonzero, the return value
   is mangled before the parameter types.  If non-NULL, DECL is
   FUNCTION_DECL for the function whose type is being emitted.

   If DECL is a member of a Java type, then a literal 'J'
   is output and the return type is mangled as if INCLUDE_RETURN_TYPE
   were nonzero.

     <bare-function-type> ::= [J]</signature/ type>+  */

static void
write_bare_function_type (const tree type, const int include_return_type_p,
			  const tree decl)
{
  int java_method_p;

  MANGLE_TRACE_TREE ("bare-function-type", type);

  /* Detect Java methods and emit special encoding.  */
  if (decl != NULL
      && DECL_FUNCTION_MEMBER_P (decl)
      && TYPE_FOR_JAVA (DECL_CONTEXT (decl))
      && !DECL_CONSTRUCTOR_P (decl)
      && !DECL_DESTRUCTOR_P (decl)
      && !DECL_CONV_FN_P (decl))
    {
      java_method_p = 1;
      write_char ('J');
    }
  else
    {
      java_method_p = 0;
    }

  /* Mangle the return type, if requested.  */
  if (include_return_type_p || java_method_p)
    write_type (TREE_TYPE (type));

  /* Now mangle the types of the arguments.  */
  ++G.parm_depth;
  write_method_parms (TYPE_ARG_TYPES (type),
		      TREE_CODE (type) == METHOD_TYPE,
		      decl);
  --G.parm_depth;
}

/* Write the mangled representation of a method parameter list of
   types given in PARM_TYPES.  If METHOD_P is nonzero, the function is
   considered a non-static method, and the this parameter is omitted.
   If non-NULL, DECL is the FUNCTION_DECL for the function whose
   parameters are being emitted.  */

static void
write_method_parms (tree parm_types, const int method_p, const tree decl)
{
  tree first_parm_type;
  tree parm_decl = decl ? DECL_ARGUMENTS (decl) : NULL_TREE;

  /* Assume this parameter type list is variable-length.  If it ends
     with a void type, then it's not.  */
  int varargs_p = 1;

  /* If this is a member function, skip the first arg, which is the
     this pointer.
       "Member functions do not encode the type of their implicit this
       parameter."

     Similarly, there's no need to mangle artificial parameters, like
     the VTT parameters for constructors and destructors.  */
  if (method_p)
    {
      parm_types = TREE_CHAIN (parm_types);
      parm_decl = parm_decl ? DECL_CHAIN (parm_decl) : NULL_TREE;

      while (parm_decl && DECL_ARTIFICIAL (parm_decl))
	{
	  parm_types = TREE_CHAIN (parm_types);
	  parm_decl = DECL_CHAIN (parm_decl);
	}
    }

  for (first_parm_type = parm_types;
       parm_types;
       parm_types = TREE_CHAIN (parm_types))
    {
      tree parm = TREE_VALUE (parm_types);
      if (parm == void_type_node)
	{
	  /* "Empty parameter lists, whether declared as () or
	     conventionally as (void), are encoded with a void parameter
	     (v)."  */
	  if (parm_types == first_parm_type)
	    write_type (parm);
	  /* If the parm list is terminated with a void type, it's
	     fixed-length.  */
	  varargs_p = 0;
	  /* A void type better be the last one.  */
	  gcc_assert (TREE_CHAIN (parm_types) == NULL);
	}
      else
	write_type (parm);
    }

  if (varargs_p)
    /* <builtin-type> ::= z  # ellipsis  */
    write_char ('z');
}

/* <class-enum-type> ::= <name>  */

static void
write_class_enum_type (const tree type)
{
  write_name (TYPE_NAME (type), /*ignore_local_scope=*/0);
}

/* Non-terminal <template-args>.  ARGS is a TREE_VEC of template
   arguments.

     <template-args> ::= I <template-arg>* E  */

static void
write_template_args (tree args)
{
  int i;
  int length = 0;

  MANGLE_TRACE_TREE ("template-args", args);

  write_char ('I');

  if (args)
    length = TREE_VEC_LENGTH (args);

  if (args && TREE_CODE (TREE_VEC_ELT (args, 0)) == TREE_VEC)
    {
      /* We have nested template args.  We want the innermost template
	 argument list.  */
      args = TREE_VEC_ELT (args, length - 1);
      length = TREE_VEC_LENGTH (args);
    }
  for (i = 0; i < length; ++i)
    write_template_arg (TREE_VEC_ELT (args, i));

  write_char ('E');
}

/* Write out the
   <unqualified-name>
   <unqualified-name> <template-args>
   part of SCOPE_REF or COMPONENT_REF mangling.  */

static void
write_member_name (tree member)
{
  if (identifier_p (member))
    write_unqualified_id (member);
  else if (DECL_P (member))
    write_unqualified_name (member);
  else if (TREE_CODE (member) == TEMPLATE_ID_EXPR)
    {
      tree name = TREE_OPERAND (member, 0);
      if (TREE_CODE (name) == OVERLOAD)
	name = OVL_FUNCTION (name);
      write_member_name (name);
      write_template_args (TREE_OPERAND (member, 1));
    }
  else
    write_expression (member);
}

/* <expression> ::= <unary operator-name> <expression>
		::= <binary operator-name> <expression> <expression>
		::= <expr-primary>

   <expr-primary> ::= <template-param>
		  ::= L <type> <value number> E		# literal
		  ::= L <mangled-name> E		# external name
		  ::= st <type>				# sizeof
		  ::= sr <type> <unqualified-name>	# dependent name
		  ::= sr <type> <unqualified-name> <template-args> */

static void
write_expression (tree expr)
{
  enum tree_code code = TREE_CODE (expr);

  /* Skip NOP_EXPRs.  They can occur when (say) a pointer argument
     is converted (via qualification conversions) to another
     type.  */
  while (TREE_CODE (expr) == NOP_EXPR
	 /* Parentheses aren't mangled.  */
	 || code == PAREN_EXPR
	 || TREE_CODE (expr) == NON_LVALUE_EXPR)
    {
      expr = TREE_OPERAND (expr, 0);
      code = TREE_CODE (expr);
    }

  if (code == BASELINK
      && (!type_unknown_p (expr)
	  || !BASELINK_QUALIFIED_P (expr)))
    {
      expr = BASELINK_FUNCTIONS (expr);
      code = TREE_CODE (expr);
    }

  /* Handle pointers-to-members by making them look like expression
     nodes.  */
  if (code == PTRMEM_CST)
    {
      expr = build_nt (ADDR_EXPR,
		       build_qualified_name (/*type=*/NULL_TREE,
					     PTRMEM_CST_CLASS (expr),
					     PTRMEM_CST_MEMBER (expr),
					     /*template_p=*/false));
      code = TREE_CODE (expr);
    }

  /* Handle template parameters.  */
  if (code == TEMPLATE_TYPE_PARM
      || code == TEMPLATE_TEMPLATE_PARM
      || code == BOUND_TEMPLATE_TEMPLATE_PARM
      || code == TEMPLATE_PARM_INDEX)
    write_template_param (expr);
  /* Handle literals.  */
  else if (TREE_CODE_CLASS (code) == tcc_constant
	   || (abi_version_at_least (2) && code == CONST_DECL))
    write_template_arg_literal (expr);
  else if (code == PARM_DECL && DECL_ARTIFICIAL (expr))
    {
      gcc_assert (!strcmp ("this", IDENTIFIER_POINTER (DECL_NAME (expr))));
      write_string ("fpT");
    }
  else if (code == PARM_DECL)
    {
      /* A function parameter used in a late-specified return type.  */
      int index = DECL_PARM_INDEX (expr);
      int level = DECL_PARM_LEVEL (expr);
      int delta = G.parm_depth - level + 1;
      gcc_assert (index >= 1);
      write_char ('f');
      if (delta != 0)
	{
	  if (abi_version_at_least (5))
	    {
	      /* Let L be the number of function prototype scopes from the
		 innermost one (in which the parameter reference occurs) up
		 to (and including) the one containing the declaration of
		 the referenced parameter.  If the parameter declaration
		 clause of the innermost function prototype scope has been
		 completely seen, it is not counted (in that case -- which
		 is perhaps the most common -- L can be zero).  */
	      write_char ('L');
	      write_unsigned_number (delta - 1);
	    }
	  else
	    G.need_abi_warning = true;
	}
      write_char ('p');
      write_compact_number (index - 1);
    }
  else if (DECL_P (expr))
    {
      /* G++ 3.2 incorrectly mangled non-type template arguments of
	 enumeration type using their names.  */
      if (code == CONST_DECL)
	G.need_abi_warning = 1;
      write_char ('L');
      write_mangled_name (expr, false);
      write_char ('E');
    }
  else if (TREE_CODE (expr) == SIZEOF_EXPR
	   && SIZEOF_EXPR_TYPE_P (expr))
    {
      write_string ("st");
      write_type (TREE_TYPE (TREE_OPERAND (expr, 0)));
    }
  else if (TREE_CODE (expr) == SIZEOF_EXPR
	   && TYPE_P (TREE_OPERAND (expr, 0)))
    {
      write_string ("st");
      write_type (TREE_OPERAND (expr, 0));
    }
  else if (TREE_CODE (expr) == ALIGNOF_EXPR
	   && TYPE_P (TREE_OPERAND (expr, 0)))
    {
      write_string ("at");
      write_type (TREE_OPERAND (expr, 0));
    }
  else if (code == SCOPE_REF
	   || code == BASELINK)
    {
      tree scope, member;
      if (code == SCOPE_REF)
	{
	  scope = TREE_OPERAND (expr, 0);
	  member = TREE_OPERAND (expr, 1);
	}
      else
	{
	  scope = BINFO_TYPE (BASELINK_ACCESS_BINFO (expr));
	  member = BASELINK_FUNCTIONS (expr);
	}

      if (!abi_version_at_least (2) && DECL_P (member))
	{
	  write_string ("sr");
	  write_type (scope);
	  /* G++ 3.2 incorrectly put out both the "sr" code and
	     the nested name of the qualified name.  */
	  G.need_abi_warning = 1;
	  write_encoding (member);
	}

      /* If the MEMBER is a real declaration, then the qualifying
	 scope was not dependent.  Ideally, we would not have a
	 SCOPE_REF in those cases, but sometimes we do.  If the second
	 argument is a DECL, then the name must not have been
	 dependent.  */
      else if (DECL_P (member))
	write_expression (member);
      else
	{
	  write_string ("sr");
	  write_type (scope);
	  write_member_name (member);
	}
    }
  else if (INDIRECT_REF_P (expr)
	   && TREE_TYPE (TREE_OPERAND (expr, 0))
	   && TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == REFERENCE_TYPE)
    {
      write_expression (TREE_OPERAND (expr, 0));
    }
  else if (identifier_p (expr))
    {
      /* An operator name appearing as a dependent name needs to be
	 specially marked to disambiguate between a use of the operator
	 name and a use of the operator in an expression.  */
      if (IDENTIFIER_OPNAME_P (expr))
	write_string ("on");
      write_unqualified_id (expr);
    }
  else if (TREE_CODE (expr) == TEMPLATE_ID_EXPR)
    {
      tree fn = TREE_OPERAND (expr, 0);
      if (is_overloaded_fn (fn))
	fn = DECL_NAME (get_first_fn (fn));
      if (IDENTIFIER_OPNAME_P (fn))
	write_string ("on");
      write_unqualified_id (fn);
      write_template_args (TREE_OPERAND (expr, 1));
    }
  else if (TREE_CODE (expr) == MODOP_EXPR)
    {
      enum tree_code subop = TREE_CODE (TREE_OPERAND (expr, 1));
      const char *name = (assignment_operator_name_info[(int) subop]
			  .mangled_name);
      write_string (name);
      write_expression (TREE_OPERAND (expr, 0));
      write_expression (TREE_OPERAND (expr, 2));
    }
  else if (code == NEW_EXPR || code == VEC_NEW_EXPR)
    {
      /* ::= [gs] nw <expression>* _ <type> E
	 ::= [gs] nw <expression>* _ <type> <initializer>
	 ::= [gs] na <expression>* _ <type> E
	 ::= [gs] na <expression>* _ <type> <initializer>
	 <initializer> ::= pi <expression>* E  */
      tree placement = TREE_OPERAND (expr, 0);
      tree type = TREE_OPERAND (expr, 1);
      tree nelts = TREE_OPERAND (expr, 2);
      tree init = TREE_OPERAND (expr, 3);
      tree t;

      gcc_assert (code == NEW_EXPR);
      if (TREE_OPERAND (expr, 2))
	code = VEC_NEW_EXPR;

      if (NEW_EXPR_USE_GLOBAL (expr))
	write_string ("gs");

      write_string (operator_name_info[(int) code].mangled_name);

      for (t = placement; t; t = TREE_CHAIN (t))
	write_expression (TREE_VALUE (t));

      write_char ('_');

      if (nelts)
	{
	  tree domain;
	  ++processing_template_decl;
	  domain = compute_array_index_type (NULL_TREE, nelts,
					     tf_warning_or_error);
	  type = build_cplus_array_type (type, domain);
	  --processing_template_decl;
	}
      write_type (type);

      if (init && TREE_CODE (init) == TREE_LIST
	  && DIRECT_LIST_INIT_P (TREE_VALUE (init)))
	write_expression (TREE_VALUE (init));
      else
	{
	  if (init)
	    write_string ("pi");
	  if (init && init != void_node)
	    for (t = init; t; t = TREE_CHAIN (t))
	      write_expression (TREE_VALUE (t));
	  write_char ('E');
	}
    }
  else if (code == DELETE_EXPR || code == VEC_DELETE_EXPR)
    {
      gcc_assert (code == DELETE_EXPR);
      if (DELETE_EXPR_USE_VEC (expr))
	code = VEC_DELETE_EXPR;

      if (DELETE_EXPR_USE_GLOBAL (expr))
	write_string ("gs");

      write_string (operator_name_info[(int) code].mangled_name);

      write_expression (TREE_OPERAND (expr, 0));
    }
  else if (code == THROW_EXPR)
    {
      tree op = TREE_OPERAND (expr, 0);
      if (op)
	{
	  write_string ("tw");
	  write_expression (op);
	}
      else
	write_string ("tr");
    }
  else if (code == CONSTRUCTOR)
    {
      vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (expr);
      unsigned i; tree val;

      if (BRACE_ENCLOSED_INITIALIZER_P (expr))
	write_string ("il");
      else
	{
	  write_string ("tl");
	  write_type (TREE_TYPE (expr));
	}
      FOR_EACH_CONSTRUCTOR_VALUE (elts, i, val)
	write_expression (val);
      write_char ('E');
    }
  else if (dependent_name (expr))
    {
      write_unqualified_id (dependent_name (expr));
    }
  else
    {
      int i, len;
      const char *name;

      /* When we bind a variable or function to a non-type template
	 argument with reference type, we create an ADDR_EXPR to show
	 the fact that the entity's address has been taken.  But, we
	 don't actually want to output a mangling code for the `&'.  */
      if (TREE_CODE (expr) == ADDR_EXPR
	  && TREE_TYPE (expr)
	  && TREE_CODE (TREE_TYPE (expr)) == REFERENCE_TYPE)
	{
	  expr = TREE_OPERAND (expr, 0);
	  if (DECL_P (expr))
	    {
	      write_expression (expr);
	      return;
	    }

	  code = TREE_CODE (expr);
	}

      if (code == COMPONENT_REF)
	{
	  tree ob = TREE_OPERAND (expr, 0);

	  if (TREE_CODE (ob) == ARROW_EXPR)
	    {
	      write_string (operator_name_info[(int)code].mangled_name);
	      ob = TREE_OPERAND (ob, 0);
	    }
	  else
	    write_string ("dt");

	  write_expression (ob);
	  write_member_name (TREE_OPERAND (expr, 1));
	  return;
	}

      /* If it wasn't any of those, recursively expand the expression.  */
      name = operator_name_info[(int) code].mangled_name;

      /* We used to mangle const_cast and static_cast like a C cast.  */
      if (!abi_version_at_least (6)
	  && (code == CONST_CAST_EXPR
	      || code == STATIC_CAST_EXPR))
	{
	  name = operator_name_info[CAST_EXPR].mangled_name;
	  G.need_abi_warning = 1;
	}

      if (name == NULL)
	{
	  switch (code)
	    {
	    case TRAIT_EXPR:
	      error ("use of built-in trait %qE in function signature; "
		     "use library traits instead", expr);
	      break;

	    default:
	      sorry ("mangling %C", code);
	      break;
	    }
	  return;
	}
      else
	write_string (name);	

      switch (code)
	{
	case CALL_EXPR:
	  {
	    tree fn = CALL_EXPR_FN (expr);

	    if (TREE_CODE (fn) == ADDR_EXPR)
	      fn = TREE_OPERAND (fn, 0);

	    /* Mangle a dependent name as the name, not whatever happens to
	       be the first function in the overload set.  */
	    if ((TREE_CODE (fn) == FUNCTION_DECL
		 || TREE_CODE (fn) == OVERLOAD)
		&& type_dependent_expression_p_push (expr))
	      fn = DECL_NAME (get_first_fn (fn));

	    write_expression (fn);
	  }

	  for (i = 0; i < call_expr_nargs (expr); ++i)
	    write_expression (CALL_EXPR_ARG (expr, i));
	  write_char ('E');
	  break;

	case CAST_EXPR:
	  write_type (TREE_TYPE (expr));
	  if (list_length (TREE_OPERAND (expr, 0)) == 1)	  
	    write_expression (TREE_VALUE (TREE_OPERAND (expr, 0)));
	  else
	    {
	      tree args = TREE_OPERAND (expr, 0);
	      write_char ('_');
	      for (; args; args = TREE_CHAIN (args))
		write_expression (TREE_VALUE (args));
	      write_char ('E');
	    }
	  break;

	case DYNAMIC_CAST_EXPR:
	case REINTERPRET_CAST_EXPR:
	case STATIC_CAST_EXPR:
	case CONST_CAST_EXPR:
	  write_type (TREE_TYPE (expr));
	  write_expression (TREE_OPERAND (expr, 0));
	  break;

	case PREINCREMENT_EXPR:
	case PREDECREMENT_EXPR:
	  if (abi_version_at_least (6))
	    write_char ('_');
	  else
	    G.need_abi_warning = 1;
	  /* Fall through.  */

	default:
	  /* In the middle-end, some expressions have more operands than
	     they do in templates (and mangling).  */
	  len = cp_tree_operand_length (expr);

	  for (i = 0; i < len; ++i)
	    {
	      tree operand = TREE_OPERAND (expr, i);
	      /* As a GNU extension, the middle operand of a
		 conditional may be omitted.  Since expression
		 manglings are supposed to represent the input token
		 stream, there's no good way to mangle such an
		 expression without extending the C++ ABI.  */
	      if (code == COND_EXPR && i == 1 && !operand)
		{
		  error ("omitted middle operand to %<?:%> operand "
			 "cannot be mangled");
		  continue;
		}
	      write_expression (operand);
	    }
	}
    }
}

/* Literal subcase of non-terminal <template-arg>.

     "Literal arguments, e.g. "A<42L>", are encoded with their type
     and value. Negative integer values are preceded with "n"; for
     example, "A<-42L>" becomes "1AILln42EE". The bool value false is
     encoded as 0, true as 1."  */

static void
write_template_arg_literal (const tree value)
{
  write_char ('L');
  write_type (TREE_TYPE (value));

  /* Write a null member pointer value as (type)0, regardless of its
     real representation.  */
  if (null_member_pointer_value_p (value))
    write_integer_cst (integer_zero_node);
  else
    switch (TREE_CODE (value))
      {
      case CONST_DECL:
	write_integer_cst (DECL_INITIAL (value));
	break;

      case INTEGER_CST:
	gcc_assert (!same_type_p (TREE_TYPE (value), boolean_type_node)
		    || integer_zerop (value) || integer_onep (value));
	write_integer_cst (value);
	break;

      case REAL_CST:
	write_real_cst (value);
	break;

      case COMPLEX_CST:
	if (TREE_CODE (TREE_REALPART (value)) == INTEGER_CST
	    && TREE_CODE (TREE_IMAGPART (value)) == INTEGER_CST)
	  {
	    write_integer_cst (TREE_REALPART (value));
	    write_char ('_');
	    write_integer_cst (TREE_IMAGPART (value));
	  }
	else if (TREE_CODE (TREE_REALPART (value)) == REAL_CST
		 && TREE_CODE (TREE_IMAGPART (value)) == REAL_CST)
	  {
	    write_real_cst (TREE_REALPART (value));
	    write_char ('_');
	    write_real_cst (TREE_IMAGPART (value));
	  }
	else
	  gcc_unreachable ();
	break;

      case STRING_CST:
	sorry ("string literal in function template signature");
	break;

      default:
	gcc_unreachable ();
      }

  write_char ('E');
}

/* Non-terminal <template-arg>.

     <template-arg> ::= <type>				# type
		    ::= L <type> </value/ number> E	# literal
		    ::= LZ <name> E			# external name
		    ::= X <expression> E		# expression  */

static void
write_template_arg (tree node)
{
  enum tree_code code = TREE_CODE (node);

  MANGLE_TRACE_TREE ("template-arg", node);

  /* A template template parameter's argument list contains TREE_LIST
     nodes of which the value field is the actual argument.  */
  if (code == TREE_LIST)
    {
      node = TREE_VALUE (node);
      /* If it's a decl, deal with its type instead.  */
      if (DECL_P (node))
	{
	  node = TREE_TYPE (node);
	  code = TREE_CODE (node);
	}
    }

  if (TREE_CODE (node) == NOP_EXPR
      && TREE_CODE (TREE_TYPE (node)) == REFERENCE_TYPE)
    {
      /* Template parameters can be of reference type. To maintain
	 internal consistency, such arguments use a conversion from
	 address of object to reference type.  */
      gcc_assert (TREE_CODE (TREE_OPERAND (node, 0)) == ADDR_EXPR);
      if (abi_version_at_least (2))
	node = TREE_OPERAND (TREE_OPERAND (node, 0), 0);
      else
	G.need_abi_warning = 1;
    }

  if (TREE_CODE (node) == BASELINK
      && !type_unknown_p (node))
    {
      if (abi_version_at_least (6))
	node = BASELINK_FUNCTIONS (node);
      else
	/* We wrongly wrapped a class-scope function in X/E.  */
	G.need_abi_warning = 1;
    }

  if (ARGUMENT_PACK_P (node))
    {
      /* Expand the template argument pack. */
      tree args = ARGUMENT_PACK_ARGS (node);
      int i, length = TREE_VEC_LENGTH (args);
      if (abi_version_at_least (6))
	write_char ('J');
      else
	{
	  write_char ('I');
	  G.need_abi_warning = 1;
	}
      for (i = 0; i < length; ++i)
        write_template_arg (TREE_VEC_ELT (args, i));
      write_char ('E');
    }
  else if (TYPE_P (node))
    write_type (node);
  else if (code == TEMPLATE_DECL)
    /* A template appearing as a template arg is a template template arg.  */
    write_template_template_arg (node);
  else if ((TREE_CODE_CLASS (code) == tcc_constant && code != PTRMEM_CST)
	   || (abi_version_at_least (2) && code == CONST_DECL)
	   || null_member_pointer_value_p (node))
    write_template_arg_literal (node);
  else if (DECL_P (node))
    {
      /* Until ABI version 2, non-type template arguments of
	 enumeration type were mangled using their names.  */
      if (code == CONST_DECL && !abi_version_at_least (2))
	G.need_abi_warning = 1;
      write_char ('L');
      /* Until ABI version 3, the underscore before the mangled name
	 was incorrectly omitted.  */
      if (!abi_version_at_least (3))
	{
	  G.need_abi_warning = 1;
	  write_char ('Z');
	}
      else
	write_string ("_Z");
      write_encoding (node);
      write_char ('E');
    }
  else
    {
      /* Template arguments may be expressions.  */
      write_char ('X');
      write_expression (node);
      write_char ('E');
    }
}

/*  <template-template-arg>
			::= <name>
			::= <substitution>  */

static void
write_template_template_arg (const tree decl)
{
  MANGLE_TRACE_TREE ("template-template-arg", decl);

  if (find_substitution (decl))
    return;
  write_name (decl, /*ignore_local_scope=*/0);
  add_substitution (decl);
}


/* Non-terminal <array-type>.  TYPE is an ARRAY_TYPE.

     <array-type> ::= A [</dimension/ number>] _ </element/ type>
		  ::= A <expression> _ </element/ type>

     "Array types encode the dimension (number of elements) and the
     element type. For variable length arrays, the dimension (but not
     the '_' separator) is omitted."  */

static void
write_array_type (const tree type)
{
  write_char ('A');
  if (TYPE_DOMAIN (type))
    {
      tree index_type;
      tree max;

      index_type = TYPE_DOMAIN (type);
      /* The INDEX_TYPE gives the upper and lower bounds of the
	 array.  */
      max = TYPE_MAX_VALUE (index_type);
      if (TREE_CODE (max) == INTEGER_CST)
	{
	  /* The ABI specifies that we should mangle the number of
	     elements in the array, not the largest allowed index.  */
	  offset_int wmax = wi::to_offset (max) + 1;
	  /* Truncate the result - this will mangle [0, SIZE_INT_MAX]
	     number of elements as zero.  */
	  wmax = wi::zext (wmax, TYPE_PRECISION (TREE_TYPE (max)));
	  gcc_assert (wi::fits_uhwi_p (wmax));
	  write_unsigned_number (wmax.to_uhwi ());
	}
      else
	{
	  max = TREE_OPERAND (max, 0);
	  if (!abi_version_at_least (2))
	    {
	      /* value_dependent_expression_p presumes nothing is
		 dependent when PROCESSING_TEMPLATE_DECL is zero.  */
	      ++processing_template_decl;
	      if (!value_dependent_expression_p (max))
		G.need_abi_warning = 1;
	      --processing_template_decl;
	    }
	  write_expression (max);
	}

    }
  write_char ('_');
  write_type (TREE_TYPE (type));
}

/* Non-terminal <pointer-to-member-type> for pointer-to-member
   variables.  TYPE is a pointer-to-member POINTER_TYPE.

     <pointer-to-member-type> ::= M </class/ type> </member/ type>  */

static void
write_pointer_to_member_type (const tree type)
{
  write_char ('M');
  write_type (TYPE_PTRMEM_CLASS_TYPE (type));
  write_type (TYPE_PTRMEM_POINTED_TO_TYPE (type));
}

/* Non-terminal <template-param>.  PARM is a TEMPLATE_TYPE_PARM,
   TEMPLATE_TEMPLATE_PARM, BOUND_TEMPLATE_TEMPLATE_PARM or a
   TEMPLATE_PARM_INDEX.

     <template-param> ::= T </parameter/ number> _  */

static void
write_template_param (const tree parm)
{
  int parm_index;

  MANGLE_TRACE_TREE ("template-parm", parm);

  switch (TREE_CODE (parm))
    {
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
      parm_index = TEMPLATE_TYPE_IDX (parm);
      break;

    case TEMPLATE_PARM_INDEX:
      parm_index = TEMPLATE_PARM_IDX (parm);
      break;

    default:
      gcc_unreachable ();
    }

  write_char ('T');
  /* NUMBER as it appears in the mangling is (-1)-indexed, with the
     earliest template param denoted by `_'.  */
  write_compact_number (parm_index);
}

/*  <template-template-param>
			::= <template-param>
			::= <substitution>  */

static void
write_template_template_param (const tree parm)
{
  tree templ = NULL_TREE;

  /* PARM, a TEMPLATE_TEMPLATE_PARM, is an instantiation of the
     template template parameter.  The substitution candidate here is
     only the template.  */
  if (TREE_CODE (parm) == BOUND_TEMPLATE_TEMPLATE_PARM)
    {
      templ
	= TI_TEMPLATE (TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (parm));
      if (find_substitution (templ))
	return;
    }

  /* <template-param> encodes only the template parameter position,
     not its template arguments, which is fine here.  */
  write_template_param (parm);
  if (templ)
    add_substitution (templ);
}

/* Non-terminal <substitution>.

      <substitution> ::= S <seq-id> _
		     ::= S_  */

static void
write_substitution (const int seq_id)
{
  MANGLE_TRACE ("substitution", "");

  write_char ('S');
  if (seq_id > 0)
    write_number (seq_id - 1, /*unsigned=*/1, 36);
  write_char ('_');
}

/* Start mangling ENTITY.  */

static inline void
start_mangling (const tree entity)
{
  G.entity = entity;
  G.need_abi_warning = false;
  obstack_free (&name_obstack, name_base);
  mangle_obstack = &name_obstack;
  name_base = obstack_alloc (&name_obstack, 0);
}

/* Done with mangling. If WARN is true, and the name of G.entity will
   be mangled differently in a future version of the ABI, issue a
   warning.  */

static void
finish_mangling_internal (const bool warn)
{
  if (warn_abi && warn && G.need_abi_warning)
    warning (OPT_Wabi, "the mangled name of %qD will change in a future "
	     "version of GCC",
	     G.entity);

  /* Clear all the substitutions.  */
  vec_safe_truncate (G.substitutions, 0);

  /* Null-terminate the string.  */
  write_char ('\0');
}


/* Like finish_mangling_internal, but return the mangled string.  */

static inline const char *
finish_mangling (const bool warn)
{
  finish_mangling_internal (warn);
  return (const char *) obstack_finish (mangle_obstack);
}

/* Like finish_mangling_internal, but return an identifier.  */

static tree
finish_mangling_get_identifier (const bool warn)
{
  finish_mangling_internal (warn);
  /* Don't obstack_finish here, and the next start_mangling will
     remove the identifier.  */
  return get_identifier ((const char *) obstack_base (mangle_obstack));
}

/* Initialize data structures for mangling.  */

void
init_mangle (void)
{
  gcc_obstack_init (&name_obstack);
  name_base = obstack_alloc (&name_obstack, 0);
  vec_alloc (G.substitutions, 0);

  /* Cache these identifiers for quick comparison when checking for
     standard substitutions.  */
  subst_identifiers[SUBID_ALLOCATOR] = get_identifier ("allocator");
  subst_identifiers[SUBID_BASIC_STRING] = get_identifier ("basic_string");
  subst_identifiers[SUBID_CHAR_TRAITS] = get_identifier ("char_traits");
  subst_identifiers[SUBID_BASIC_ISTREAM] = get_identifier ("basic_istream");
  subst_identifiers[SUBID_BASIC_OSTREAM] = get_identifier ("basic_ostream");
  subst_identifiers[SUBID_BASIC_IOSTREAM] = get_identifier ("basic_iostream");
}

/* Generate the mangled name of DECL.  */

static tree
mangle_decl_string (const tree decl)
{
  tree result;
  location_t saved_loc = input_location;
  tree saved_fn = NULL_TREE;
  bool template_p = false;

  /* We shouldn't be trying to mangle an uninstantiated template.  */
  gcc_assert (!type_dependent_expression_p (decl));

  if (DECL_LANG_SPECIFIC (decl) && DECL_USE_TEMPLATE (decl))
    {
      struct tinst_level *tl = current_instantiation ();
      if ((!tl || tl->decl != decl)
	  && push_tinst_level (decl))
	{
	  template_p = true;
	  saved_fn = current_function_decl;
	  current_function_decl = NULL_TREE;
	}
    }
  input_location = DECL_SOURCE_LOCATION (decl);

  start_mangling (decl);

  if (TREE_CODE (decl) == TYPE_DECL)
    write_type (TREE_TYPE (decl));
  else
    write_mangled_name (decl, true);

  result = finish_mangling_get_identifier (/*warn=*/true);
  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_decl_string = '%s'\n\n",
	     IDENTIFIER_POINTER (result));

  if (template_p)
    {
      pop_tinst_level ();
      current_function_decl = saved_fn;
    }
  input_location = saved_loc;

  return result;
}

/* Return an identifier for the external mangled name of DECL.  */

static tree
get_mangled_id (tree decl)
{
  tree id = mangle_decl_string (decl);
  return targetm.mangle_decl_assembler_name (decl, id);
}

/* Create an identifier for the external mangled name of DECL.  */

void
mangle_decl (const tree decl)
{
  tree id;
  bool dep;

  /* Don't bother mangling uninstantiated templates.  */
  ++processing_template_decl;
  if (TREE_CODE (decl) == TYPE_DECL)
    dep = dependent_type_p (TREE_TYPE (decl));
  else
    dep = (DECL_LANG_SPECIFIC (decl) && DECL_TEMPLATE_INFO (decl)
	   && any_dependent_template_arguments_p (DECL_TI_ARGS (decl)));
  --processing_template_decl;
  if (dep)
    return;

  id = get_mangled_id (decl);
  SET_DECL_ASSEMBLER_NAME (decl, id);

  if (G.need_abi_warning
      /* Don't do this for a fake symbol we aren't going to emit anyway.  */
      && TREE_CODE (decl) != TYPE_DECL
      && !DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (decl)
      && !DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (decl))
    {
#ifdef ASM_OUTPUT_DEF
      /* If the mangling will change in the future, emit an alias with the
	 future mangled name for forward-compatibility.  */
      int save_ver;
      tree id2, alias;
#endif

      SET_IDENTIFIER_GLOBAL_VALUE (id, decl);
      if (IDENTIFIER_GLOBAL_VALUE (id) != decl)
	inform (DECL_SOURCE_LOCATION (decl), "-fabi-version=6 (or =0) "
		"avoids this error with a change in mangling");

#ifdef ASM_OUTPUT_DEF
      save_ver = flag_abi_version;
      flag_abi_version = 0;
      id2 = mangle_decl_string (decl);
      id2 = targetm.mangle_decl_assembler_name (decl, id2);
      flag_abi_version = save_ver;

      alias = make_alias_for (decl, id2);
      DECL_IGNORED_P (alias) = 1;
      TREE_PUBLIC (alias) = TREE_PUBLIC (decl);
      DECL_VISIBILITY (alias) = DECL_VISIBILITY (decl);
      if (vague_linkage_p (decl))
	DECL_WEAK (alias) = 1;
      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  /* Don't create an alias to an unreferenced function.  */
	  if (struct cgraph_node *n = cgraph_get_node (decl))
	    cgraph_same_body_alias (n, alias, decl);
	}
      else
	varpool_extra_name_alias (alias, decl);
#endif
    }
}

/* Generate the mangled representation of TYPE.  */

const char *
mangle_type_string (const tree type)
{
  const char *result;

  start_mangling (type);
  write_type (type);
  result = finish_mangling (/*warn=*/false);
  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_type_string = '%s'\n\n", result);
  return result;
}

/* Create an identifier for the mangled name of a special component
   for belonging to TYPE.  CODE is the ABI-specified code for this
   component.  */

static tree
mangle_special_for_type (const tree type, const char *code)
{
  tree result;

  /* We don't have an actual decl here for the special component, so
     we can't just process the <encoded-name>.  Instead, fake it.  */
  start_mangling (type);

  /* Start the mangling.  */
  write_string ("_Z");
  write_string (code);

  /* Add the type.  */
  write_type (type);
  result = finish_mangling_get_identifier (/*warn=*/false);

  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_special_for_type = %s\n\n",
	     IDENTIFIER_POINTER (result));

  return result;
}

/* Create an identifier for the mangled representation of the typeinfo
   structure for TYPE.  */

tree
mangle_typeinfo_for_type (const tree type)
{
  return mangle_special_for_type (type, "TI");
}

/* Create an identifier for the mangled name of the NTBS containing
   the mangled name of TYPE.  */

tree
mangle_typeinfo_string_for_type (const tree type)
{
  return mangle_special_for_type (type, "TS");
}

/* Create an identifier for the mangled name of the vtable for TYPE.  */

tree
mangle_vtbl_for_type (const tree type)
{
  return mangle_special_for_type (type, "TV");
}

/* Returns an identifier for the mangled name of the VTT for TYPE.  */

tree
mangle_vtt_for_type (const tree type)
{
  return mangle_special_for_type (type, "TT");
}

/* Return an identifier for a construction vtable group.  TYPE is
   the most derived class in the hierarchy; BINFO is the base
   subobject for which this construction vtable group will be used.

   This mangling isn't part of the ABI specification; in the ABI
   specification, the vtable group is dumped in the same COMDAT as the
   main vtable, and is referenced only from that vtable, so it doesn't
   need an external name.  For binary formats without COMDAT sections,
   though, we need external names for the vtable groups.

   We use the production

    <special-name> ::= CT <type> <offset number> _ <base type>  */

tree
mangle_ctor_vtbl_for_type (const tree type, const tree binfo)
{
  tree result;

  start_mangling (type);

  write_string ("_Z");
  write_string ("TC");
  write_type (type);
  write_integer_cst (BINFO_OFFSET (binfo));
  write_char ('_');
  write_type (BINFO_TYPE (binfo));

  result = finish_mangling_get_identifier (/*warn=*/false);
  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_ctor_vtbl_for_type = %s\n\n",
	     IDENTIFIER_POINTER (result));
  return result;
}

/* Mangle a this pointer or result pointer adjustment.

   <call-offset> ::= h <fixed offset number> _
		 ::= v <fixed offset number> _ <virtual offset number> _ */

static void
mangle_call_offset (const tree fixed_offset, const tree virtual_offset)
{
  write_char (virtual_offset ? 'v' : 'h');

  /* For either flavor, write the fixed offset.  */
  write_integer_cst (fixed_offset);
  write_char ('_');

  /* For a virtual thunk, add the virtual offset.  */
  if (virtual_offset)
    {
      write_integer_cst (virtual_offset);
      write_char ('_');
    }
}

/* Return an identifier for the mangled name of a this-adjusting or
   covariant thunk to FN_DECL.  FIXED_OFFSET is the initial adjustment
   to this used to find the vptr.  If VIRTUAL_OFFSET is non-NULL, this
   is a virtual thunk, and it is the vtbl offset in
   bytes. THIS_ADJUSTING is nonzero for a this adjusting thunk and
   zero for a covariant thunk. Note, that FN_DECL might be a covariant
   thunk itself. A covariant thunk name always includes the adjustment
   for the this pointer, even if there is none.

   <special-name> ::= T <call-offset> <base encoding>
		  ::= Tc <this_adjust call-offset> <result_adjust call-offset>
					<base encoding>  */

tree
mangle_thunk (tree fn_decl, const int this_adjusting, tree fixed_offset,
	      tree virtual_offset)
{
  tree result;

  start_mangling (fn_decl);

  write_string ("_Z");
  write_char ('T');

  if (!this_adjusting)
    {
      /* Covariant thunk with no this adjustment */
      write_char ('c');
      mangle_call_offset (integer_zero_node, NULL_TREE);
      mangle_call_offset (fixed_offset, virtual_offset);
    }
  else if (!DECL_THUNK_P (fn_decl))
    /* Plain this adjusting thunk.  */
    mangle_call_offset (fixed_offset, virtual_offset);
  else
    {
      /* This adjusting thunk to covariant thunk.  */
      write_char ('c');
      mangle_call_offset (fixed_offset, virtual_offset);
      fixed_offset = ssize_int (THUNK_FIXED_OFFSET (fn_decl));
      virtual_offset = THUNK_VIRTUAL_OFFSET (fn_decl);
      if (virtual_offset)
	virtual_offset = BINFO_VPTR_FIELD (virtual_offset);
      mangle_call_offset (fixed_offset, virtual_offset);
      fn_decl = THUNK_TARGET (fn_decl);
    }

  /* Scoped name.  */
  write_encoding (fn_decl);

  result = finish_mangling_get_identifier (/*warn=*/false);
  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_thunk = %s\n\n", IDENTIFIER_POINTER (result));
  return result;
}

/* This hash table maps TYPEs to the IDENTIFIER for a conversion
   operator to TYPE.  The nodes are IDENTIFIERs whose TREE_TYPE is the
   TYPE.  */

static GTY ((param_is (union tree_node))) htab_t conv_type_names;

/* Hash a node (VAL1) in the table.  */

static hashval_t
hash_type (const void *val)
{
  return (hashval_t) TYPE_UID (TREE_TYPE ((const_tree) val));
}

/* Compare VAL1 (a node in the table) with VAL2 (a TYPE).  */

static int
compare_type (const void *val1, const void *val2)
{
  return TREE_TYPE ((const_tree) val1) == (const_tree) val2;
}

/* Return an identifier for the mangled unqualified name for a
   conversion operator to TYPE.  This mangling is not specified by the
   ABI spec; it is only used internally.  */

tree
mangle_conv_op_name_for_type (const tree type)
{
  void **slot;
  tree identifier;

  if (type == error_mark_node)
    return error_mark_node;

  if (conv_type_names == NULL)
    conv_type_names = htab_create_ggc (31, &hash_type, &compare_type, NULL);

  slot = htab_find_slot_with_hash (conv_type_names, type,
				   (hashval_t) TYPE_UID (type), INSERT);
  identifier = (tree)*slot;
  if (!identifier)
    {
      char buffer[64];

       /* Create a unique name corresponding to TYPE.  */
      sprintf (buffer, "operator %lu",
	       (unsigned long) htab_elements (conv_type_names));
      identifier = get_identifier (buffer);
      *slot = identifier;

      /* Hang TYPE off the identifier so it can be found easily later
	 when performing conversions.  */
      TREE_TYPE (identifier) = type;

      /* Set bits on the identifier so we know later it's a conversion.  */
      IDENTIFIER_OPNAME_P (identifier) = 1;
      IDENTIFIER_TYPENAME_P (identifier) = 1;
    }

  return identifier;
}

/* Write out the appropriate string for this variable when generating
   another mangled name based on this one.  */

static void
write_guarded_var_name (const tree variable)
{
  if (DECL_NAME (variable)
      && strncmp (IDENTIFIER_POINTER (DECL_NAME (variable)), "_ZGR", 4) == 0)
    /* The name of a guard variable for a reference temporary should refer
       to the reference, not the temporary.  */
    write_string (IDENTIFIER_POINTER (DECL_NAME (variable)) + 4);
  else
    write_name (variable, /*ignore_local_scope=*/0);
}

/* Return an identifier for the name of an initialization guard
   variable for indicated VARIABLE.  */

tree
mangle_guard_variable (const tree variable)
{
  start_mangling (variable);
  write_string ("_ZGV");
  write_guarded_var_name (variable);
  return finish_mangling_get_identifier (/*warn=*/false);
}

/* Return an identifier for the name of a thread_local initialization
   function for VARIABLE.  */

tree
mangle_tls_init_fn (const tree variable)
{
  start_mangling (variable);
  write_string ("_ZTH");
  write_guarded_var_name (variable);
  return finish_mangling_get_identifier (/*warn=*/false);
}

/* Return an identifier for the name of a thread_local wrapper
   function for VARIABLE.  */

#define TLS_WRAPPER_PREFIX "_ZTW"

tree
mangle_tls_wrapper_fn (const tree variable)
{
  start_mangling (variable);
  write_string (TLS_WRAPPER_PREFIX);
  write_guarded_var_name (variable);
  return finish_mangling_get_identifier (/*warn=*/false);
}

/* Return true iff FN is a thread_local wrapper function.  */

bool
decl_tls_wrapper_p (const tree fn)
{
  if (TREE_CODE (fn) != FUNCTION_DECL)
    return false;
  tree name = DECL_NAME (fn);
  return strncmp (IDENTIFIER_POINTER (name), TLS_WRAPPER_PREFIX,
		  strlen (TLS_WRAPPER_PREFIX)) == 0;
}

/* Return an identifier for the name of a temporary variable used to
   initialize a static reference.  This isn't part of the ABI, but we might
   as well call them something readable.  */

static GTY(()) int temp_count;

tree
mangle_ref_init_variable (const tree variable)
{
  start_mangling (variable);
  write_string ("_ZGR");
  write_name (variable, /*ignore_local_scope=*/0);
  /* Avoid name clashes with aggregate initialization of multiple
     references at once.  */
  write_unsigned_number (temp_count++);
  return finish_mangling_get_identifier (/*warn=*/false);
}


/* Foreign language type mangling section.  */

/* How to write the type codes for the integer Java type.  */

static void
write_java_integer_type_codes (const tree type)
{
  if (type == java_int_type_node)
    write_char ('i');
  else if (type == java_short_type_node)
    write_char ('s');
  else if (type == java_byte_type_node)
    write_char ('c');
  else if (type == java_char_type_node)
    write_char ('w');
  else if (type == java_long_type_node)
    write_char ('x');
  else if (type == java_boolean_type_node)
    write_char ('b');
  else
    gcc_unreachable ();
}

/* Given a CLASS_TYPE, such as a record for std::bad_exception this
   function generates a mangled name for the vtable map variable of
   the class type.  For example, if the class type is
   "std::bad_exception", the mangled name for the class is
   "St13bad_exception".  This function would generate the name
   "_ZN4_VTVISt13bad_exceptionE12__vtable_mapE", which unmangles as:
   "_VTV<std::bad_exception>::__vtable_map".  */


char *
get_mangled_vtable_map_var_name (tree class_type)
{
  char *var_name = NULL;
  const char *prefix = "_ZN4_VTVI";
  const char *postfix = "E12__vtable_mapE";

  gcc_assert (TREE_CODE (class_type) == RECORD_TYPE);

  tree class_id = DECL_ASSEMBLER_NAME (TYPE_NAME (class_type));
  unsigned int len = strlen (IDENTIFIER_POINTER (class_id)) +
                     strlen (prefix) +
                     strlen (postfix) + 1;

  var_name = (char *) xmalloc (len);

  sprintf (var_name, "%s%s%s", prefix, IDENTIFIER_POINTER (class_id), postfix);

  return var_name;
}

#include "gt-cp-mangle.h"
