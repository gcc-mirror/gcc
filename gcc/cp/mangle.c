/* Name mangling for the 3.0 C++ ABI.
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
   Written by Alex Samuel <sameul@codesourcery.com>

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

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

     mangle_typeinfo_for_type:        typeinfo data
     mangle_typeinfo_string_for_type: typeinfo type name
     mangle_vtbl_for_type:            virtual table data
     mangle_vtt_for_type:             VTT data
     mangle_ctor_vtbl_for_type:       `C-in-B' constructor virtual table data
     mangle_thunk:                    thunk function or entry

*/

#include "config.h"
#include "system.h"
#include "tree.h"
#include "cp-tree.h"
#include "obstack.h"
#include "toplev.h"
#include "varray.h"

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
           (FN), tree_code_name[TREE_CODE (NODE)], (void *) (NODE))
#else
# define MANGLE_TRACE(FN, INPUT)
# define MANGLE_TRACE_TREE(FN, NODE)
#endif

/* Non-zero if NODE is a class template-id.  We can't rely on
   CLASSTYPE_USE_TEMPLATE here because of tricky bugs in the parser
   that hard to distinguish A<T> from A, where A<T> is the type as
   instantiated outside of the template, and A is the type used
   without parameters inside the template.  */
#define CLASSTYPE_TEMPLATE_ID_P(NODE)				      \
  (TYPE_LANG_SPECIFIC (NODE) != NULL 				      \
   && CLASSTYPE_TEMPLATE_INFO (NODE) != NULL                          \
   && (PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (NODE))))

/* Things we only need one of.  This module is not reentrant.  */
static struct globals
{
  /* The name in which we're building the mangled name.  */
  struct obstack name_obstack;

  /* An array of the current substitution candidates, in the order
     we've seen them.  */
  varray_type substitutions;

  /* The entity that is being mangled.  */
  tree entity;

  /* True if the mangling will be different in a future version of the
     ABI.  */
  bool need_abi_warning;
} G;

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
static tree subst_identifiers[SUBID_MAX];

/* Single-letter codes for builtin integer types, defined in
   <builtin-type>.  These are indexed by integer_type_kind values.  */
static char
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
  'y'   /* itk_unsigned_long_long */
};

static int decl_is_template_id PARAMS ((tree, tree*));

/* Functions for handling substitutions.  */

static inline tree canonicalize_for_substitution PARAMS ((tree));
static void add_substitution PARAMS ((tree));
static inline int is_std_substitution PARAMS ((tree, substitution_identifier_index_t));
static inline int is_std_substitution_char PARAMS ((tree, substitution_identifier_index_t));
static int find_substitution PARAMS ((tree));

/* Functions for emitting mangled representations of things.  */

static void write_mangled_name PARAMS ((tree));
static void write_encoding PARAMS ((tree));
static void write_name PARAMS ((tree, int));
static void write_unscoped_name PARAMS ((tree));
static void write_unscoped_template_name PARAMS ((tree));
static void write_nested_name PARAMS ((tree));
static void write_prefix PARAMS ((tree));
static void write_template_prefix PARAMS ((tree));
static void write_unqualified_name PARAMS ((tree));
static void write_source_name PARAMS ((tree));
static int hwint_to_ascii PARAMS ((unsigned HOST_WIDE_INT, unsigned int, char *, unsigned));
static void write_number PARAMS ((unsigned HOST_WIDE_INT, int,
				  unsigned int));
static void write_integer_cst PARAMS ((tree));
static void write_identifier PARAMS ((const char *));
static void write_special_name_constructor PARAMS ((tree));
static void write_special_name_destructor PARAMS ((tree));
static void write_type PARAMS ((tree));
static int write_CV_qualifiers_for_type PARAMS ((tree));
static void write_builtin_type PARAMS ((tree));
static void write_function_type PARAMS ((tree));
static void write_bare_function_type PARAMS ((tree, int, tree));
static void write_method_parms PARAMS ((tree, int, tree));
static void write_class_enum_type PARAMS ((tree));
static void write_template_args PARAMS ((tree));
static void write_expression PARAMS ((tree));
static void write_template_arg_literal PARAMS ((tree));
static void write_template_arg PARAMS ((tree));
static void write_template_template_arg PARAMS ((tree));
static void write_array_type PARAMS ((tree));
static void write_pointer_to_member_type PARAMS ((tree));
static void write_template_param PARAMS ((tree));
static void write_template_template_param PARAMS ((tree));
static void write_substitution PARAMS ((int));
static int discriminator_for_local_entity PARAMS ((tree));
static int discriminator_for_string_literal PARAMS ((tree, tree));
static void write_discriminator PARAMS ((int));
static void write_local_name PARAMS ((tree, tree, tree));
static void dump_substitution_candidates PARAMS ((void));
static const char *mangle_decl_string PARAMS ((tree));

/* Control functions.  */

static inline void start_mangling PARAMS ((tree));
static inline const char *finish_mangling PARAMS ((bool));
static tree mangle_special_for_type PARAMS ((tree, const char *));

/* Foreign language functions. */

static void write_java_integer_type_codes PARAMS ((tree));

/* Append a single character to the end of the mangled
   representation.  */
#define write_char(CHAR)                                              \
  obstack_1grow (&G.name_obstack, (CHAR))

/* Append a sized buffer to the end of the mangled representation. */
#define write_chars(CHAR, LEN)                                        \
  obstack_grow (&G.name_obstack, (CHAR), (LEN))

/* Append a NUL-terminated string to the end of the mangled
   representation.  */
#define write_string(STRING)                                          \
  obstack_grow (&G.name_obstack, (STRING), strlen (STRING))

/* Return the position at which the next character will be appended to
   the mangled representation.  */
#define mangled_position()                                              \
  obstack_object_size (&G.name_obstack)

/* Non-zero if NODE1 and NODE2 are both TREE_LIST nodes and have the
   same purpose (context, which may be a type) and value (template
   decl).  See write_template_prefix for more information on what this
   is used for.  */
#define NESTED_TEMPLATE_MATCH(NODE1, NODE2)                         \
  (TREE_CODE (NODE1) == TREE_LIST                                     \
   && TREE_CODE (NODE2) == TREE_LIST                                  \
   && ((TYPE_P (TREE_PURPOSE (NODE1))                                 \
        && same_type_p (TREE_PURPOSE (NODE1), TREE_PURPOSE (NODE2)))\
       || TREE_PURPOSE (NODE1) == TREE_PURPOSE (NODE2))             \
   && TREE_VALUE (NODE1) == TREE_VALUE (NODE2))

/* Write out a signed quantity in base 10.  */
#define write_signed_number(NUMBER) \
  write_number ((NUMBER), /*unsigned_p=*/0, 10)

/* Write out an unsigned quantity in base 10.  */
#define write_unsigned_number(NUMBER) \
  write_number ((NUMBER), /*unsigned_p=*/1, 10)

/* If DECL is a template instance, return non-zero and, if
   TEMPLATE_INFO is non-NULL, set *TEMPLATE_INFO to its template info.
   Otherwise return zero.  */

static int
decl_is_template_id (decl, template_info)
     tree decl;
     tree* template_info;
{
  if (TREE_CODE (decl) == TYPE_DECL)
    {
      /* TYPE_DECLs are handled specially.  Look at its type to decide
	 if this is a template instantiation.  */
      tree type = TREE_TYPE (decl);

      if (CLASS_TYPE_P (type) && CLASSTYPE_TEMPLATE_ID_P (type))
	{
	  if (template_info != NULL)
	    /* For a templated TYPE_DECL, the template info is hanging
	       off the type.  */
	    *template_info = CLASSTYPE_TEMPLATE_INFO (type);
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
dump_substitution_candidates ()
{
  unsigned i;

  fprintf (stderr, "  ++ substitutions  ");
  for (i = 0; i < VARRAY_ACTIVE_SIZE (G.substitutions); ++i)
    {
      tree el = VARRAY_TREE (G.substitutions, i);
      const char *name = "???";

      if (i > 0)
	fprintf (stderr, "                    ");
      if (DECL_P (el))
	name = IDENTIFIER_POINTER (DECL_NAME (el));
      else if (TREE_CODE (el) == TREE_LIST)
	name = IDENTIFIER_POINTER (DECL_NAME (TREE_VALUE (el)));
      else if (TYPE_NAME (el))
	name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (el)));
      fprintf (stderr, " S%d_ = ", i - 1);
      if (TYPE_P (el) && 
	  (CP_TYPE_RESTRICT_P (el) 
	   || CP_TYPE_VOLATILE_P (el) 
	   || CP_TYPE_CONST_P (el)))
	fprintf (stderr, "CV-");
      fprintf (stderr, "%s (%s at %p)\n", 
	       name, tree_code_name[TREE_CODE (el)], (void *) el);
    }
}

/* Both decls and types can be substitution candidates, but sometimes
   they refer to the same thing.  For instance, a TYPE_DECL and
   RECORD_TYPE for the same class refer to the same thing, and should
   be treated accordinginly in substitutions.  This function returns a
   canonicalized tree node representing NODE that is used when adding
   and substitution candidates and finding matches.  */

static inline tree
canonicalize_for_substitution (node)
     tree node;
{
  /* For a TYPE_DECL, use the type instead.  */
  if (TREE_CODE (node) == TYPE_DECL)
    node = TREE_TYPE (node);
  if (TYPE_P (node))
    node = canonical_type_variant (node);

  return node;
}

/* Add NODE as a substitution candidate.  NODE must not already be on
   the list of candidates.  */

static void
add_substitution (node)
     tree node;
{
  tree c;

  if (DEBUG_MANGLE)
    fprintf (stderr, "  ++ add_substitution (%s at %10p)\n", 
	     tree_code_name[TREE_CODE (node)], (void *) node);

  /* Get the canonicalized substitution candidate for NODE.  */
  c = canonicalize_for_substitution (node);
  if (DEBUG_MANGLE && c != node)
    fprintf (stderr, "  ++ using candidate (%s at %10p)\n",
	     tree_code_name[TREE_CODE (node)], (void *) node);
  node = c;

#if ENABLE_CHECKING
  /* Make sure NODE isn't already a candidate.  */
  {
    int i;
    for (i = VARRAY_ACTIVE_SIZE (G.substitutions); --i >= 0; )
      {
	tree candidate = VARRAY_TREE (G.substitutions, i);
	if ((DECL_P (node) 
	     && node == candidate)
	    || (TYPE_P (node) 
		&& TYPE_P (candidate) 
		&& same_type_p (node, candidate)))
	  abort ();
      }
  }
#endif /* ENABLE_CHECKING */

  /* Put the decl onto the varray of substitution candidates.  */
  VARRAY_PUSH_TREE (G.substitutions, node);

  if (DEBUG_MANGLE)
    dump_substitution_candidates ();
}

/* Helper function for find_substitution.  Returns non-zero if NODE,
   which may be a decl or a CLASS_TYPE, is a template-id with template
   name of substitution_index[INDEX] in the ::std namespace.  */

static inline int 
is_std_substitution (node, index)
     tree node;
     substitution_identifier_index_t index;
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
	  && CLASSTYPE_TEMPLATE_INFO (type)
	  && (DECL_NAME (CLASSTYPE_TI_TEMPLATE (type)) 
	      == subst_identifiers[index]));
}

/* Helper function for find_substitution.  Returns non-zero if NODE,
   which may be a decl or a CLASS_TYPE, is the template-id
   ::std::identifier<char>, where identifier is
   substitution_index[INDEX].  */

static inline int
is_std_substitution_char (node, index)
     tree node;
     substitution_identifier_index_t index;
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
   return non-zero.  If none is found, just return zero.  */

static int
find_substitution (node)
     tree node;
{
  int i;
  int size = VARRAY_ACTIVE_SIZE (G.substitutions);
  tree decl;
  tree type;

  if (DEBUG_MANGLE)
    fprintf (stderr, "  ++ find_substitution (%s at %p)\n",
	     tree_code_name[TREE_CODE (node)], (void *) node);

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
	  && same_type_p (TREE_VEC_ELT (args, 0), char_type_node)
	  && is_std_substitution_char (TREE_VEC_ELT (args, 1),
				       SUBID_CHAR_TRAITS))
	{
	  /* Got them.  Is this basic_istream?  */
	  tree name = DECL_NAME (CLASSTYPE_TI_TEMPLATE (type));
	  if (name == subst_identifiers[SUBID_BASIC_ISTREAM])
	    {
	      write_string ("Si");
	      return 1;
	    }
	  /* Or basic_ostream?  */
	  else if (name == subst_identifiers[SUBID_BASIC_OSTREAM])
	    {
	      write_string ("So");
	      return 1;
	    }
	  /* Or basic_iostream?  */
	  else if (name == subst_identifiers[SUBID_BASIC_IOSTREAM])
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
     operation.    */
  for (i = 0; i < size; ++i)
    {
      tree candidate = VARRAY_TREE (G.substitutions, i);
      /* NODE is a matched to a candidate if it's the same decl node or
	 if it's the same type.  */
      if (decl == candidate
	  || (TYPE_P (candidate) && type && TYPE_P (type)
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


/*  <mangled-name>      ::= _Z <encoding>  */

static inline void
write_mangled_name (decl)
     tree decl;
{
  MANGLE_TRACE_TREE ("mangled-name", decl);

  if (DECL_LANG_SPECIFIC (decl)
      && DECL_EXTERN_C_FUNCTION_P (decl)
      && ! DECL_OVERLOADED_OPERATOR_P (decl))
    /* The standard notes:
         "The <encoding> of an extern "C" function is treated like
	 global-scope data, i.e. as its <source-name> without a type."
       We cannot write overloaded operators that way though,
       because it contains characters invalid in assembler.  */
    write_source_name (DECL_NAME (decl));
  else
    /* C++ name; needs to be mangled.  */
    {
      write_string ("_Z");
      write_encoding (decl);
    }
}

/*   <encoding>		::= <function name> <bare-function-type>
			::= <data name>  */

static void
write_encoding (decl)
     tree decl;
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

      if (decl_is_template_id (decl, NULL))
	fn_type = get_mostly_instantiated_function_type (decl, NULL, NULL);
      else
	fn_type = TREE_TYPE (decl);

      write_bare_function_type (fn_type, 
				(!DECL_CONSTRUCTOR_P (decl)
				 && !DECL_DESTRUCTOR_P (decl)
				 && !DECL_CONV_FN_P (decl)
				 && decl_is_template_id (decl, NULL)),
				decl);
    }
}

/* <name> ::= <unscoped-name>
          ::= <unscoped-template-name> <template-args>
	  ::= <nested-name>
	  ::= <local-name>  

   If IGNORE_LOCAL_SCOPE is non-zero, this production of <name> is
   called from <local-name>, which mangles the enclosing scope
   elsewhere and then uses this function to mangle just the part
   underneath the function scope.  So don't use the <local-name>
   production, to avoid an infinite recursion.  */

static void
write_name (decl, ignore_local_scope)
     tree decl;
     int ignore_local_scope;
{
  tree context;

  MANGLE_TRACE_TREE ("name", decl);

  if (TREE_CODE (decl) == TYPE_DECL)
    {
      /* In case this is a typedef, fish out the corresponding
	 TYPE_DECL for the main variant.  */
      decl = TYPE_NAME (TYPE_MAIN_VARIANT (TREE_TYPE (decl)));
      context = TYPE_CONTEXT (TYPE_MAIN_VARIANT (TREE_TYPE (decl)));
    }
  else
    context = (DECL_CONTEXT (decl) == NULL) ? NULL : CP_DECL_CONTEXT (decl);

  /* A decl in :: or ::std scope is treated specially.  The former is
     mangled using <unscoped-name> or <unscoped-template-name>, the
     latter with a special substitution.  Also, a name that is
     directly in a local function scope is also mangled with
     <unscoped-name> rather than a full <nested-name>.  */
  if (context == NULL 
      || context == global_namespace 
      || DECL_NAMESPACE_STD_P (context)
      || (ignore_local_scope && TREE_CODE (context) == FUNCTION_DECL))
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
	  while (context != NULL && context != global_namespace)
	    {
	      /* Make sure we're always dealing with decls.  */
	      if (context != NULL && TYPE_P (context))
		context = TYPE_NAME (context);
	      /* Is this a function?  */
	      if (TREE_CODE (context) == FUNCTION_DECL)
		{
		  /* Yes, we have local scope.  Use the <local-name>
		     production for the innermost function scope.  */
		  write_local_name (context, local_entity, decl);
		  return;
		}
	      /* Up one scope level.  */
	      local_entity = context;
	      context = CP_DECL_CONTEXT (context);
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
write_unscoped_name (decl)
     tree decl;
{
  tree context = CP_DECL_CONTEXT (decl);

  MANGLE_TRACE_TREE ("unscoped-name", decl);

  /* Is DECL in ::std?  */
  if (DECL_NAMESPACE_STD_P (context))
    {
      write_string ("St");
      write_unqualified_name (decl);
    }
  /* If not, it should be either in the global namespace, or directly
     in a local function scope.  */
  else if (context == global_namespace 
	   || context == NULL
	   || TREE_CODE (context) == FUNCTION_DECL)
    write_unqualified_name (decl);
  else 
    abort ();
}

/* <unscoped-template-name> ::= <unscoped-name>
                            ::= <substitution>  */

static void
write_unscoped_template_name (decl)
     tree decl;
{
  MANGLE_TRACE_TREE ("unscoped-template-name", decl);

  if (find_substitution (decl))
    return;
  write_unscoped_name (decl);
  add_substitution (decl);
}

/* Write the nested name, including CV-qualifiers, of DECL.

   <nested-name> ::= N [<CV-qualifiers>] <prefix> <unqualified-name> E  
                 ::= N [<CV-qualifiers>] <template-prefix> <template-args> E

   <CV-qualifiers> ::= [r] [V] [K]  */

static void
write_nested_name (decl)
     tree decl;
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
    }

  /* Is this a template instance?  */
  if (decl_is_template_id (decl, &template_info))
    {
      /* Yes, use <template-prefix>.  */
      write_template_prefix (decl);
      write_template_args (TI_ARGS (template_info));
    }
  else
    {
      /* No, just use <prefix>  */
      write_prefix (DECL_CONTEXT (decl));
      write_unqualified_name (decl);
    }
  write_char ('E');
}

/* <prefix> ::= <prefix> <unqualified-name>>
            ::= <template-prefix> <template-args>
	    ::= # empty
	    ::= <substitution>  */

static void
write_prefix (node)
     tree node;
{
  tree decl;
  /* Non-NULL if NODE represents a template-id.  */
  tree template_info = NULL;

  MANGLE_TRACE_TREE ("prefix", node);

  if (node == NULL
      || node == global_namespace)
    return;

  if (find_substitution (node))
    return;

  if (DECL_P (node))
    /* Node is a decl.  */
    {
      /* If this is a function decl, that means we've hit function
	 scope, so this prefix must be for a local name.  In this
	 case, we're under the <local-name> production, which encodes
	 the enclosing function scope elsewhere.  So don't continue
	 here.  */
      if (TREE_CODE (node) == FUNCTION_DECL)
	return;

      decl = node;
      decl_is_template_id (decl, &template_info);
    }
  else
    /* Node is a type.  */
    {
      decl = TYPE_NAME (node);
      if (CLASSTYPE_TEMPLATE_ID_P (node))
	template_info = CLASSTYPE_TEMPLATE_INFO (node);
    }

  /* In G++ 3.2, the name of the template parameter was used.  */
  if (TREE_CODE (node) == TEMPLATE_TYPE_PARM)
    G.need_abi_warning = true;

  if (template_info != NULL)
    /* Templated.  */
    {
      write_template_prefix (decl);
      write_template_args (TI_ARGS (template_info));
    }
  else
    /* Not templated.  */
    {
      write_prefix (CP_DECL_CONTEXT (decl));
      write_unqualified_name (decl);
    }

  add_substitution (node);
}

/* <template-prefix> ::= <prefix> <template component>
                     ::= <substitution>  */

static void
write_template_prefix (node)
     tree node;
{
  tree decl = DECL_P (node) ? node : TYPE_NAME (node);
  tree type = DECL_P (node) ? TREE_TYPE (node) : node;
  tree context = CP_DECL_CONTEXT (decl);
  tree template_info;
  tree template;
  tree substitution;

  MANGLE_TRACE_TREE ("template-prefix", node);

  /* Find the template decl.  */
  if (decl_is_template_id (decl, &template_info))
    template = TI_TEMPLATE (template_info);
  else if (CLASSTYPE_TEMPLATE_ID_P (type))
    template = CLASSTYPE_TI_TEMPLATE (type);
  else
    /* Oops, not a template.  */
    abort ();

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
    substitution = build_tree_list (context, template);
  else
    substitution = template;

  if (find_substitution (substitution))
    return;

  /* In G++ 3.2, the name of the template template parameter was used.  */
  if (TREE_CODE (TREE_TYPE (template)) == TEMPLATE_TEMPLATE_PARM)
    G.need_abi_warning = true;

  write_prefix (context);
  write_unqualified_name (decl);

  add_substitution (substitution);
}

/* We don't need to handle thunks, vtables, or VTTs here.  Those are
   mangled through special entry points.  

    <unqualified-name>  ::= <operator-name>
			::= <special-name>  
			::= <source-name>  */

static void
write_unqualified_name (decl)
     tree decl;
{
  MANGLE_TRACE_TREE ("unqualified-name", decl);

  if (DECL_LANG_SPECIFIC (decl) != NULL && DECL_CONSTRUCTOR_P (decl))
    write_special_name_constructor (decl);
  else if (DECL_LANG_SPECIFIC (decl) != NULL && DECL_DESTRUCTOR_P (decl))
    write_special_name_destructor (decl);
  else if (DECL_CONV_FN_P (decl)) 
    {
      /* Conversion operator. Handle it right here.  
           <operator> ::= cv <type>  */
      tree type;
      if (decl_is_template_id (decl, NULL))
	{
	  tree fn_type = get_mostly_instantiated_function_type (decl, NULL,
								NULL);
	  type = TREE_TYPE (fn_type);
	}
      else
	type = TREE_TYPE (DECL_NAME (decl));
      write_string ("cv");
      write_type (type);
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
  else
    write_source_name (DECL_NAME (decl));
}

/* Non-termial <source-name>.  IDENTIFIER is an IDENTIFIER_NODE.  

     <source-name> ::= </length/ number> <identifier>  */

static void
write_source_name (identifier)
     tree identifier;
{
  MANGLE_TRACE_TREE ("source-name", identifier);

  /* Never write the whole template-id name including the template
     arguments; we only want the template name.  */
  if (IDENTIFIER_TEMPLATE (identifier))
    identifier = IDENTIFIER_TEMPLATE (identifier);

  write_unsigned_number (IDENTIFIER_LENGTH (identifier));
  write_identifier (IDENTIFIER_POINTER (identifier));
}

/* Convert NUMBER to ascii using base BASE and generating at least
   MIN_DIGITS characters. BUFFER points to the _end_ of the buffer
   into which to store the characters. Returns the number of
   characters generated (these will be layed out in advance of where
   BUFFER points).  */

static int
hwint_to_ascii (number, base, buffer, min_digits)
     unsigned HOST_WIDE_INT number;
     unsigned int base;
     char *buffer;
     unsigned min_digits;
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
write_number (number, unsigned_p, base)
     unsigned HOST_WIDE_INT number;
     int unsigned_p;
     unsigned int base;
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
   bigger than that, which we must deal with. */

static inline void
write_integer_cst (cst)
     tree cst;
{
  int sign = tree_int_cst_sgn (cst);

  if (TREE_INT_CST_HIGH (cst) + (sign < 0))
    {
      /* A bignum. We do this in chunks, each of which fits in a
	 HOST_WIDE_INT. */
      char buffer[sizeof (HOST_WIDE_INT) * 8 * 2];
      unsigned HOST_WIDE_INT chunk;
      unsigned chunk_digits;
      char *ptr = buffer + sizeof (buffer);
      unsigned count = 0;
      tree n, base, type;
      int done;

      /* HOST_WIDE_INT must be at least 32 bits, so 10^9 is
	 representable. */
      chunk = 1000000000;
      chunk_digits = 9;
      
      if (sizeof (HOST_WIDE_INT) >= 8)
	{
	  /* It is at least 64 bits, so 10^18 is representable. */
	  chunk_digits = 18;
	  chunk *= chunk;
	}
      
      type = signed_or_unsigned_type (1, TREE_TYPE (cst));
      base = build_int_2 (chunk, 0);
      n = build_int_2 (TREE_INT_CST_LOW (cst), TREE_INT_CST_HIGH (cst));
      TREE_TYPE (n) = TREE_TYPE (base) = type;

      if (sign < 0)
	{
	  write_char ('n');
	  n = fold (build1 (NEGATE_EXPR, type, n));
	}
      do
	{
	  tree d = fold (build (FLOOR_DIV_EXPR, type, n, base));
	  tree tmp = fold (build (MULT_EXPR, type, d, base));
	  unsigned c;
	  
	  done = integer_zerop (d);
	  tmp = fold (build (MINUS_EXPR, type, n, tmp));
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
      unsigned HOST_WIDE_INT low = TREE_INT_CST_LOW (cst);
      
      if (sign < 0)
	{
	  write_char ('n');
	  low = -low;
	}
      write_unsigned_number (low);
    }
}

/* Non-terminal <identifier>.

     <identifier> ::= </unqualified source code identifier>  */

static void
write_identifier (identifier)
     const char *identifier;
{
  MANGLE_TRACE ("identifier", identifier);
  write_string (identifier);
}

/* Handle constructor productions of non-terminal <special-name>.
   CTOR is a constructor FUNCTION_DECL. 

     <special-name> ::= C1   # complete object constructor
                    ::= C2   # base object constructor
                    ::= C3   # complete object allocating constructor

   Currently, allocating constructors are never used. 

   We also need to provide mangled names for the maybe-in-charge
   constructor, so we treat it here too.  mangle_decl_string will
   append *INTERNAL* to that, to make sure we never emit it.  */

static void
write_special_name_constructor (ctor)
     tree ctor;
{
  if (DECL_COMPLETE_CONSTRUCTOR_P (ctor)
      /* Even though we don't ever emit a definition of the
	 old-style destructor, we still have to consider entities
	 (like static variables) nested inside it.  */
      || DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (ctor))
    write_string ("C1");
  else if (DECL_BASE_CONSTRUCTOR_P (ctor))
    write_string ("C2");
  else
    abort ();
}

/* Handle destructor productions of non-terminal <special-name>.
   DTOR is a destructor FUNCTION_DECL. 

     <special-name> ::= D0 # deleting (in-charge) destructor
                    ::= D1 # complete object (in-charge) destructor
                    ::= D2 # base object (not-in-charge) destructor

   We also need to provide mangled names for the maybe-incharge
   destructor, so we treat it here too.  mangle_decl_string will
   append *INTERNAL* to that, to make sure we never emit it.  */

static void
write_special_name_destructor (dtor)
     tree dtor;
{
  if (DECL_DELETING_DESTRUCTOR_P (dtor))
    write_string ("D0");
  else if (DECL_COMPLETE_DESTRUCTOR_P (dtor)
	   /* Even though we don't ever emit a definition of the
	      old-style destructor, we still have to consider entities
	      (like static variables) nested inside it.  */
	   || DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (dtor))
    write_string ("D1");
  else if (DECL_BASE_DESTRUCTOR_P (dtor))
    write_string ("D2");
  else
    abort ();
}

/* Return the discriminator for ENTITY appearing inside
   FUNCTION.  The discriminator is the lexical ordinal of VAR among
   entities with the same name in the same FUNCTION.  */

static int
discriminator_for_local_entity (entity)
     tree entity;
{
  tree *type;
  int discriminator;

  /* Assume this is the only local entity with this name.  */
  discriminator = 0;

  if (DECL_DISCRIMINATOR_P (entity) && DECL_LANG_SPECIFIC (entity))
    discriminator = DECL_DISCRIMINATOR (entity);
  else if (TREE_CODE (entity) == TYPE_DECL)
    {
      /* Scan the list of local classes.  */
      entity = TREE_TYPE (entity);
      for (type = &VARRAY_TREE (local_classes, 0); *type != entity; ++type)
        if (TYPE_IDENTIFIER (*type) == TYPE_IDENTIFIER (entity)
            && TYPE_CONTEXT (*type) == TYPE_CONTEXT (entity))
	  ++discriminator;
    }  

  return discriminator;
}

/* Return the discriminator for STRING, a string literal used inside
   FUNCTION.  The disciminator is the lexical ordinal of STRING among
   string literals used in FUNCTION.  */

static int
discriminator_for_string_literal (function, string)
     tree function ATTRIBUTE_UNUSED;
     tree string ATTRIBUTE_UNUSED;
{
  /* For now, we don't discriminate amongst string literals.  */
  return 0;
}

/*   <discriminator> := _ <number>   

   The discriminator is used only for the second and later occurrences
   of the same name within a single function. In this case <number> is
   n - 2, if this is the nth occurrence, in lexical order.  */

static void
write_discriminator (discriminator)
     int discriminator;
{
  /* If discriminator is zero, don't write anything.  Otherwise... */
  if (discriminator > 0)
    {
      write_char ('_');
      write_unsigned_number (discriminator - 1);
    }
}

/* Mangle the name of a function-scope entity.  FUNCTION is the
   FUNCTION_DECL for the enclosing function.  ENTITY is the decl for
   the entity itself.  LOCAL_ENTITY is the entity that's directly
   scoped in FUNCTION_DECL, either ENTITY itself or an enclosing scope
   of ENTITY.

     <local-name> := Z <function encoding> E <entity name> [<discriminator>]
                  := Z <function encoding> E s [<discriminator>]  */

static void
write_local_name (function, local_entity, entity)
     tree function;
     tree local_entity;
     tree entity;
{
  MANGLE_TRACE_TREE ("local-name", entity);

  write_char ('Z');
  write_encoding (function);
  write_char ('E');
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

   TYPE is a type node.  */

static void 
write_type (type)
     tree type;
{
  /* This gets set to non-zero if TYPE turns out to be a (possibly
     CV-qualified) builtin type.  */
  int is_builtin_type = 0;

  MANGLE_TRACE_TREE ("type", type);

  if (type == error_mark_node)
    return;

  if (find_substitution (type))
    return;
  
  if (write_CV_qualifiers_for_type (type) > 0)
    /* If TYPE was CV-qualified, we just wrote the qualifiers; now
       mangle the unqualified type.  The recursive call is needed here
       since both the qualified and uqualified types are substitution
       candidates.  */
    write_type (TYPE_MAIN_VARIANT (type));
  else if (TREE_CODE (type) == ARRAY_TYPE)
    /* It is important not to use the TYPE_MAIN_VARIANT of TYPE here
       so that the cv-qualification of the element type is available
       in write_array_type.  */
    write_array_type (type);
  else
    {
      /* See through any typedefs.  */
      type = TYPE_MAIN_VARIANT (type);

      switch (TREE_CODE (type))
	{
	case VOID_TYPE:
	case BOOLEAN_TYPE:
	case INTEGER_TYPE:  /* Includes wchar_t.  */
	case REAL_TYPE:
	  /* If this is a typedef, TYPE may not be one of
	     the standard builtin type nodes, but an alias of one.  Use
	     TYPE_MAIN_VARIANT to get to the underlying builtin type.  */
	  write_builtin_type (TYPE_MAIN_VARIANT (type));
	  ++is_builtin_type;
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
	  /* A pointer-to-member variable is represented by a POINTER_TYPE
	     to an OFFSET_TYPE, so check for this first.  */
	  if (TYPE_PTRMEM_P (type))
	    write_pointer_to_member_type (type);
	  else
	    {
	      write_char ('P');
	      write_type (TREE_TYPE (type));
	    }
	  break;

	case REFERENCE_TYPE:
	  write_char ('R');
	  write_type (TREE_TYPE (type));
	  break;

	case TEMPLATE_TYPE_PARM:
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

	case OFFSET_TYPE:
	  write_pointer_to_member_type (build_pointer_type (type));
	  break;

	case VECTOR_TYPE:
	  write_string ("U8__vector");
	  write_type (TREE_TYPE (type));
	  break;

	default:
	  abort ();
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
write_CV_qualifiers_for_type (type)
     tree type;
{
  int num_qualifiers = 0;

  /* The order is specified by:

       "In cases where multiple order-insensitive qualifiers are
       present, they should be ordered 'K' (closest to the base type),
       'V', 'r', and 'U' (farthest from the base type) ..."  

     Note that we do not use cp_type_quals below; given "const
     int[3]", the "const" is emitted with the "int", not with the
     array.  */

  if (TYPE_QUALS (type) & TYPE_QUAL_RESTRICT)
    {
      write_char ('r');
      ++num_qualifiers;
    }
  if (TYPE_QUALS (type) & TYPE_QUAL_VOLATILE)
    {
      write_char ('V');
      ++num_qualifiers;
    }
  if (TYPE_QUALS (type) & TYPE_QUAL_CONST)
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
write_builtin_type (type)
     tree type;
{
  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
      write_char ('v');
      break;

    case BOOLEAN_TYPE:
      write_char ('b');
      break;

    case INTEGER_TYPE:
      /* If this is size_t, get the underlying int type.  */
      if (TYPE_IS_SIZETYPE (type))
	type = TYPE_DOMAIN (type);

      /* TYPE may still be wchar_t, since that isn't in
	 integer_type_nodes.  */
      if (type == wchar_type_node)
	write_char ('w');
      else if (TYPE_FOR_JAVA (type))
	write_java_integer_type_codes (type);
      else
	{
	  size_t itk;
	  /* Assume TYPE is one of the shared integer type nodes.  Find
	     it in the array of these nodes.  */
	iagain:
	  for (itk = 0; itk < itk_none; ++itk)
	    if (type == integer_types[itk])
	      {
		/* Print the corresponding single-letter code.  */
		write_char (integer_type_codes[itk]);
		break;
	      }

	  if (itk == itk_none)
	    {
	      tree t = type_for_mode (TYPE_MODE (type), TREE_UNSIGNED (type));
	      if (type == t)
		{
		  if (TYPE_PRECISION (type) == 128)
		    write_char (TREE_UNSIGNED (type) ? 'o' : 'n');
		  else
		    /* Couldn't find this type.  */
		    abort ();
		}
	      else
		{
		  type = t;
		  goto iagain;
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
      else
	abort ();
      break;

    default:
      abort ();
    }
}

/* Non-terminal <function-type>.  NODE is a FUNCTION_TYPE or
   METHOD_TYPE.  The return type is mangled before the parameter
   types.

     <function-type> ::= F [Y] <bare-function-type> E   */

static void
write_function_type (type)
     tree type;
{
  MANGLE_TRACE_TREE ("function-type", type);

  /* For a pointer to member function, the function type may have
     cv-qualifiers, indicating the quals for the artificial 'this'
     parameter.  */
  if (TREE_CODE (type) == METHOD_TYPE)
    {
      /* The first parameter must be a POINTER_TYPE pointing to the
	 `this' parameter.  */
      tree this_type = TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (type)));
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
  write_char ('E');
}

/* Non-terminal <bare-function-type>.  TYPE is a FUNCTION_TYPE or
   METHOD_TYPE.  If INCLUDE_RETURN_TYPE is non-zero, the return value
   is mangled before the parameter types.  If non-NULL, DECL is
   FUNCTION_DECL for the function whose type is being emitted.

     <bare-function-type> ::= </signature/ type>+  */

static void
write_bare_function_type (type, include_return_type_p, decl)
     tree type;
     int include_return_type_p;
     tree decl;
{
  MANGLE_TRACE_TREE ("bare-function-type", type);

  /* Mangle the return type, if requested.  */
  if (include_return_type_p)
    write_type (TREE_TYPE (type));

  /* Now mangle the types of the arguments.  */
  write_method_parms (TYPE_ARG_TYPES (type), 
		      TREE_CODE (type) == METHOD_TYPE,
		      decl);
}

/* Write the mangled representation of a method parameter list of
   types given in PARM_TYPES.  If METHOD_P is non-zero, the function is 
   considered a non-static method, and the this parameter is omitted.
   If non-NULL, DECL is the FUNCTION_DECL for the function whose
   parameters are being emitted.  */

static void
write_method_parms (parm_types, method_p, decl)
     tree decl;
     tree parm_types;
     int method_p;
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
      parm_decl = parm_decl ? TREE_CHAIN (parm_decl) : NULL_TREE;

      while (parm_decl && DECL_ARTIFICIAL (parm_decl))
	{
	  parm_types = TREE_CHAIN (parm_types);
	  parm_decl = TREE_CHAIN (parm_decl);
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
	  my_friendly_assert (TREE_CHAIN (parm_types) == NULL, 20000523);
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
write_class_enum_type (type)
     tree type;
{
  write_name (TYPE_NAME (type), /*ignore_local_scope=*/0);
}

/* Non-terminal <template-args>.  ARGS is a TREE_VEC of template
   arguments.

     <template-args> ::= I <template-arg>+ E  */

static void
write_template_args (args)
     tree args;
{
  int i;
  int length = TREE_VEC_LENGTH (args);

  MANGLE_TRACE_TREE ("template-args", args);

  my_friendly_assert (length > 0, 20000422);

  if (TREE_CODE (TREE_VEC_ELT (args, 0)) == TREE_VEC)
    {
      /* We have nested template args.  We want the innermost template
	 argument list.  */
      args = TREE_VEC_ELT (args, length - 1);
      length = TREE_VEC_LENGTH (args);
    }

  write_char ('I');
  for (i = 0; i < length; ++i)
    write_template_arg (TREE_VEC_ELT (args, i));
  write_char ('E');
}

/* <expression> ::= <unary operator-name> <expression>
		::= <binary operator-name> <expression> <expression>
		::= <expr-primary>

   <expr-primary> ::= <template-param>
		  ::= L <type> <value number> E  # literal
		  ::= L <mangled-name> E         # external name  */

static void
write_expression (expr)
     tree expr;
{
  enum tree_code code;

  code = TREE_CODE (expr);

  /* Handle pointers-to-members by making them look like expression
     nodes.  */
  if (code == PTRMEM_CST)
    {
      expr = build_nt (ADDR_EXPR,
		       build_nt (SCOPE_REF,
				 PTRMEM_CST_CLASS (expr),
				 PTRMEM_CST_MEMBER (expr)));
      code = TREE_CODE (expr);
    }

  /* Skip NOP_EXPRs.  They can occur when (say) a pointer argument
     is converted (via qualification conversions) to another
     type.  */
  while (TREE_CODE (expr) == NOP_EXPR
	 || TREE_CODE (expr) == NON_LVALUE_EXPR)
    {
      expr = TREE_OPERAND (expr, 0);
      code = TREE_CODE (expr);
    }

  /* Handle template parameters. */
  if (code == TEMPLATE_TYPE_PARM 
      || code == TEMPLATE_TEMPLATE_PARM
      || code == BOUND_TEMPLATE_TEMPLATE_PARM
      || code == TEMPLATE_PARM_INDEX)
    write_template_param (expr);
  /* Handle literals.  */
  else if (TREE_CODE_CLASS (code) == 'c')
    write_template_arg_literal (expr);
  else if (DECL_P (expr))
    {
      /* G++ 3.2 incorrectly mangled non-type template arguments of
	 enumeration type using their names.  */
      if (code == CONST_DECL)
	G.need_abi_warning = 1;
      write_char ('L');
      write_mangled_name (expr);
      write_char ('E');
    }
  else if (TREE_CODE (expr) == SIZEOF_EXPR 
	   && TYPE_P (TREE_OPERAND (expr, 0)))
    {
      write_string ("st");
      write_type (TREE_OPERAND (expr, 0));
    }
  else
    {
      int i;

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
      
      /* If it wasn't any of those, recursively expand the expression.  */
      write_string (operator_name_info[(int) code].mangled_name);

      switch (code)
	{
	case CAST_EXPR:
	  write_type (TREE_TYPE (expr));
	  write_expression (TREE_VALUE (TREE_OPERAND (expr, 0)));
	  break;

	case STATIC_CAST_EXPR:
	case CONST_CAST_EXPR:
	  write_type (TREE_TYPE (expr));
	  write_expression (TREE_OPERAND (expr, 0));
	  break;

	  
	/* Handle pointers-to-members specially.  */
	case SCOPE_REF:
	  write_type (TREE_OPERAND (expr, 0));
	  if (TREE_CODE (TREE_OPERAND (expr, 1)) == IDENTIFIER_NODE)
	    write_source_name (TREE_OPERAND (expr, 1));
	  else
	    {
	      /* G++ 3.2 incorrectly put out both the "sr" code and
		 the nested name of the qualified name.  */
	      G.need_abi_warning = 1;
	      write_encoding (TREE_OPERAND (expr, 1));
	    }
	  break;

	default:
	  for (i = 0; i < TREE_CODE_LENGTH (code); ++i)
	    write_expression (TREE_OPERAND (expr, i));
	}
    }
}

/* Literal subcase of non-terminal <template-arg>.  

     "Literal arguments, e.g. "A<42L>", are encoded with their type
     and value. Negative integer values are preceded with "n"; for
     example, "A<-42L>" becomes "1AILln42EE". The bool value false is
     encoded as 0, true as 1. If floating-point arguments are accepted
     as an extension, their values should be encoded using a
     fixed-length lowercase hexadecimal string corresponding to the
     internal representation (IEEE on IA-64), high-order bytes first,
     without leading zeroes. For example: "Lfbff000000E" is -1.0f."  */

static void
write_template_arg_literal (value)
     tree value;
{
  tree type = TREE_TYPE (value);
  write_char ('L');
  write_type (type);

  if (TREE_CODE (value) == CONST_DECL)
    write_integer_cst (DECL_INITIAL (value));
  else if (TREE_CODE (value) == INTEGER_CST)
    {
      if (same_type_p (type, boolean_type_node))
	{
	  if (value == boolean_false_node || integer_zerop (value))
	    write_unsigned_number (0);
	  else if (value == boolean_true_node)
	    write_unsigned_number (1);
	  else 
	    abort ();
	}
      else
	write_integer_cst (value);
    }
  else if (TREE_CODE (value) == REAL_CST)
    {
#ifdef CROSS_COMPILE
      static int explained;

      if (!explained) 
	{
	  sorry ("real-valued template parameters when cross-compiling");
	  explained = 1;
	}
#else
      size_t i;
      for (i = 0; i < sizeof (TREE_REAL_CST (value)); ++i)
	write_number (((unsigned char *) 
		       &TREE_REAL_CST (value))[i], 
		      /*unsigned_p=*/1,
		      16);
#endif
    }
  else
    abort ();

  write_char ('E');
}

/* Non-terminal <tempalate-arg>.  

     <template-arg> ::= <type>                        # type
                    ::= L <type> </value/ number> E   # literal
                    ::= LZ <name> E                   # external name
                    ::= X <expression> E              # expression  */

static void
write_template_arg (node)
     tree node;
{
  enum tree_code code = TREE_CODE (node);

  MANGLE_TRACE_TREE ("template-arg", node);

  /* A template template paramter's argument list contains TREE_LIST
     nodes of which the value field is the the actual argument.  */
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

  if (TYPE_P (node))
    write_type (node);
  else if (code == TEMPLATE_DECL)
    /* A template appearing as a template arg is a template template arg.  */
    write_template_template_arg (node);
  else if (DECL_P (node))
    {
      /* G++ 3.2 incorrectly mangled non-type template arguments of
	 enumeration type using their names.  */
      if (code == CONST_DECL)
	G.need_abi_warning = 1;
      write_char ('L');
      write_char ('Z');
      write_encoding (node);
      write_char ('E');
    }
  else if (TREE_CODE_CLASS (code) == 'c' && code != PTRMEM_CST)
    write_template_arg_literal (node);
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

void
write_template_template_arg (tree decl)
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
write_array_type (type)
  tree type;
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
	  max = size_binop (PLUS_EXPR, max, size_one_node);
	  write_unsigned_number (tree_low_cst (max, 1));
	}
      else
	write_expression (TREE_OPERAND (max, 0));
    }
  write_char ('_');
  write_type (TREE_TYPE (type));
}

/* Non-terminal <pointer-to-member-type> for pointer-to-member
   variables.  TYPE is a pointer-to-member POINTER_TYPE.

     <pointer-to-member-type> ::= M </class/ type> </member/ type>  */

static void
write_pointer_to_member_type (type)
     tree type;
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
write_template_param (parm)
     tree parm;
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
      abort ();
    }

  write_char ('T');
  /* NUMBER as it appears in the mangling is (-1)-indexed, with the
     earliest template param denoted by `_'.  */
  if (parm_index > 0)
    write_unsigned_number (parm_index - 1);
  write_char ('_');
}

/*  <template-template-param>
                        ::= <template-param> 
			::= <substitution>  */

static void
write_template_template_param (parm)
     tree parm;
{
  tree template = NULL_TREE;

  /* PARM, a TEMPLATE_TEMPLATE_PARM, is an instantiation of the
     template template parameter.  The substitution candidate here is
     only the template.  */
  if (TREE_CODE (parm) == BOUND_TEMPLATE_TEMPLATE_PARM)
    {
      template 
	= TI_TEMPLATE (TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (parm));
      if (find_substitution (template))
	return;
    }

  /* <template-param> encodes only the template parameter position,
     not its template arguments, which is fine here.  */
  write_template_param (parm);
  if (template)
    add_substitution (template);
}

/* Non-terminal <substitution>.  

      <substitution> ::= S <seq-id> _
                     ::= S_  */

static void
write_substitution (seq_id)
     int seq_id;
{
  MANGLE_TRACE ("substitution", "");

  write_char ('S');
  if (seq_id > 0)
    write_number (seq_id - 1, /*unsigned=*/1, 36);
  write_char ('_');
}

/* Start mangling a new name or type.  */

static inline void
start_mangling (tree entity)
{
  G.entity = entity;
  G.need_abi_warning = false;
  obstack_free (&G.name_obstack, obstack_base (&G.name_obstack));
}

/* Done with mangling.  Return the generated mangled name.  */

static inline const char *
finish_mangling (bool warn)
{
  if (warn_abi && warn && G.need_abi_warning)
    warning ("the mangled name of `%D' will change in a future "
	     "version of GCC",
	     G.entity);

  /* Clear all the substitutions.  */
  VARRAY_POP_ALL (G.substitutions);

  /* Null-terminate the string.  */
  write_char ('\0');

  return (const char *) obstack_base (&G.name_obstack);
}

/* Initialize data structures for mangling.  */

void
init_mangle ()
{
  gcc_obstack_init (&G.name_obstack);
  VARRAY_TREE_INIT (G.substitutions, 1, "mangling substitutions");

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

static const char *
mangle_decl_string (decl)
     tree decl;
{
  const char *result;

  start_mangling (decl);

  if (TREE_CODE (decl) == TYPE_DECL)
    write_type (TREE_TYPE (decl));
  else if (/* The names of `extern "C"' functions are not mangled.  */
	   (DECL_EXTERN_C_FUNCTION_P (decl)
	    /* But overloaded operator names *are* mangled.  */
	    && !DECL_OVERLOADED_OPERATOR_P (decl))
	   /* The names of global variables aren't mangled either.  */
	   || (TREE_CODE (decl) == VAR_DECL
	       && CP_DECL_CONTEXT (decl) == global_namespace)
	   /* And neither are `extern "C"' variables.  */
	   || (TREE_CODE (decl) == VAR_DECL
	       && DECL_EXTERN_C_P (decl)))
    write_string (IDENTIFIER_POINTER (DECL_NAME (decl)));
  else
    {
      write_mangled_name (decl);
      if (DECL_LANG_SPECIFIC (decl)
	  && (DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (decl)
	      || DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (decl)))
	/* We need a distinct mangled name for these entities, but
	   we should never actually output it.  So, we append some
	   characters the assembler won't like.  */
	write_string (" *INTERNAL* ");
    }

  result = finish_mangling (/*warn=*/true);
  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_decl_string = '%s'\n\n", result);
  return result;
}

/* Create an identifier for the external mangled name of DECL.  */

void
mangle_decl (decl)
     tree decl;
{
  tree id = get_identifier (mangle_decl_string (decl));

  SET_DECL_ASSEMBLER_NAME (decl, id);
}

/* Generate the mangled representation of TYPE.  */

const char *
mangle_type_string (type)
     tree type;
{
  const char *result;

  start_mangling (type);
  write_type (type);
  result = finish_mangling (/*warn=*/false);
  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_type_string = '%s'\n\n", result);
  return result;
}

/* Create an identifier for the mangled representation of TYPE.  */

tree
mangle_type (type)
     tree type;
{
  return get_identifier (mangle_type_string (type));
}

/* Create an identifier for the mangled name of a special component
   for belonging to TYPE.  CODE is the ABI-specified code for this
   component.  */

static tree
mangle_special_for_type (type, code)
     tree type;
     const char *code;
{
  const char *result;

  /* We don't have an actual decl here for the special component, so
     we can't just process the <encoded-name>.  Instead, fake it.  */
  start_mangling (type);

  /* Start the mangling.  */
  write_string ("_Z");
  write_string (code);

  /* Add the type.  */
  write_type (type);
  result = finish_mangling (/*warn=*/false);

  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_special_for_type = %s\n\n", result);

  return get_identifier (result);
}

/* Create an identifier for the mangled representation of the typeinfo
   structure for TYPE.  */

tree
mangle_typeinfo_for_type (type)
     tree type;
{
  return mangle_special_for_type (type, "TI");
}

/* Create an identifier for the mangled name of the NTBS containing
   the mangled name of TYPE.  */

tree
mangle_typeinfo_string_for_type (type)
     tree type;
{
  return mangle_special_for_type (type, "TS");
}

/* Create an identifier for the mangled name of the vtable for TYPE.  */

tree
mangle_vtbl_for_type (type)
     tree type;
{
  return mangle_special_for_type (type, "TV");
}

/* Returns an identifier for the mangled name of the VTT for TYPE.  */

tree
mangle_vtt_for_type (type)
     tree type;
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
mangle_ctor_vtbl_for_type (type, binfo)
     tree type;
     tree binfo;
{
  const char *result;

  start_mangling (type);

  write_string ("_Z");
  write_string ("TC");
  write_type (type);
  write_integer_cst (BINFO_OFFSET (binfo));
  write_char ('_');
  write_type (BINFO_TYPE (binfo));

  result = finish_mangling (/*warn=*/false);
  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_ctor_vtbl_for_type = %s\n\n", result);
  return get_identifier (result);
}

/* Return an identifier for the mangled name of a thunk to FN_DECL.
   OFFSET is the initial adjustment to this used to find the vptr.  If
   VCALL_OFFSET is non-NULL, this is a virtual thunk, and it is the
   vtbl offset in bytes.  

    <special-name> ::= Th <offset number> _ <base encoding>
                   ::= Tv <offset number> _ <vcall offset number> _
		                                            <base encoding>
*/

tree
mangle_thunk (fn_decl, offset, vcall_offset)
     tree fn_decl;
     tree offset;
     tree vcall_offset;
{
  const char *result;
  
  start_mangling (fn_decl);

  write_string ("_Z");
  /* The <special-name> for virtual thunks is Tv, for non-virtual
     thunks Th.  */
  write_char ('T');
  if (vcall_offset != 0)
    write_char ('v');
  else
    write_char ('h');

  /* For either flavor, write the offset to this.  */
  write_integer_cst (offset);
  write_char ('_');

  /* For a virtual thunk, add the vcall offset.  */
  if (vcall_offset)
    {
      /* Virtual thunk.  Write the vcall offset and base type name.  */
      write_integer_cst (vcall_offset);
      write_char ('_');
    }

  /* Scoped name.  */
  write_encoding (fn_decl);

  result = finish_mangling (/*warn=*/false);
  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_thunk = %s\n\n", result);
  return get_identifier (result);
}

/* Return an identifier for the mangled unqualified name for a
   conversion operator to TYPE.  This mangling is not specified by the
   ABI spec; it is only used internally.  */

tree
mangle_conv_op_name_for_type (type)
     tree type;
{
  tree identifier;

  /* Build the mangling for TYPE.  */
  const char *mangled_type = mangle_type_string (type);
  /* Allocate a temporary buffer for the complete name.  */
  char *op_name = concat ("operator ", mangled_type, NULL);
  /* Find or create an identifier.  */
  identifier = get_identifier (op_name);
  /* Done with the temporary buffer.  */
  free (op_name);
  /* Set bits on the identifier so we know later it's a conversion.  */
  IDENTIFIER_OPNAME_P (identifier) = 1;
  IDENTIFIER_TYPENAME_P (identifier) = 1;
  /* Hang TYPE off the identifier so it can be found easily later when
     performing conversions.  */
  TREE_TYPE (identifier) = type;

  return identifier;
}

/* Return an identifier for the name of an initialization guard
   variable for indicated VARIABLE.  */

tree
mangle_guard_variable (variable)
     tree variable;
{
  start_mangling (variable);
  write_string ("_ZGV");
  if (strncmp (IDENTIFIER_POINTER (DECL_NAME (variable)), "_ZGR", 4) == 0)
    /* The name of a guard variable for a reference temporary should refer
       to the reference, not the temporary.  */
    write_string (IDENTIFIER_POINTER (DECL_NAME (variable)) + 4);
  else
    write_name (variable, /*ignore_local_scope=*/0);
  return get_identifier (finish_mangling (/*warn=*/false));
}

/* Return an identifier for the name of a temporary variable used to
   initialize a static reference.  This isn't part of the ABI, but we might
   as well call them something readable.  */

tree
mangle_ref_init_variable (variable)
     tree variable;
{
  start_mangling (variable);
  write_string ("_ZGR");
  write_name (variable, /*ignore_local_scope=*/0);
  return get_identifier (finish_mangling (/*warn=*/false));
}


/* Foreign language type mangling section.  */

/* How to write the type codes for the integer Java type.  */

static void
write_java_integer_type_codes (type)
     tree type;
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
    abort ();
}

