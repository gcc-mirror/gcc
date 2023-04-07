/* m2decl.cc provides an interface to GCC decl trees.

Copyright (C) 2012-2023 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "gcc-consolidation.h"

#include "../gm2-lang.h"
#include "../m2-tree.h"

#define m2decl_c
#include "m2assert.h"
#include "m2block.h"
#include "m2decl.h"
#include "m2expr.h"
#include "m2tree.h"
#include "m2treelib.h"
#include "m2type.h"
#include "m2convert.h"

extern GTY (()) tree current_function_decl;

/* Used in BuildStartFunctionType.  */
static GTY (()) tree param_type_list;
static GTY (()) tree param_list = NULL_TREE; /* Ready for the next time we
                                                call/define a function.  */
#if 0
tree
m2decl_DeclareM2linkStaticInitialization (location_t location,
					  int ScaffoldStatic)
{
  m2block_pushGlobalScope ();
  /* Generate: int M2LINK_StaticInitialization = ScaffoldStatic;  */
  tree init = m2decl_BuildIntegerConstant (ScaffoldStatic);
  tree static_init = m2decl_DeclareKnownVariable (location, "m2pim_M2LINK_StaticInitialization",
						  integer_type_node,
						  TRUE, FALSE, FALSE, TRUE, NULL_TREE, init);
  m2block_popGlobalScope ();
  return static_init;
}


tree
m2decl_DeclareM2linkForcedModuleInitOrder (location_t location,
					   const char *RuntimeOverride)
{
  m2block_pushGlobalScope ();
  /* Generate: const char *ForcedModuleInitOrder = RuntimeOverride;  */
  tree ptr_to_char = build_pointer_type (char_type_node);
  TYPE_READONLY (ptr_to_char) = TRUE;
  tree init = m2decl_BuildPtrToTypeString (location, RuntimeOverride, ptr_to_char);
  tree forced_order = m2decl_DeclareKnownVariable (location, "m2pim_M2LINK_ForcedModuleInitOrder",
						   ptr_to_char,
						   TRUE, FALSE, FALSE, TRUE, NULL_TREE, init);
  m2block_popGlobalScope ();
  return forced_order;
}
#endif


/* DeclareKnownVariable declares a variable to GCC.  */

tree
m2decl_DeclareKnownVariable (location_t location, const char *name, tree type,
                             bool exported, bool imported, bool istemporary,
                             bool isglobal, tree scope, tree initial)
{
  tree id;
  tree decl;

  m2assert_AssertLocation (location);
  ASSERT (m2tree_is_type (type), type);
  ASSERT_BOOL (isglobal);

  id = get_identifier (name);
  type = m2tree_skip_type_decl (type);
  decl = build_decl (location, VAR_DECL, id, type);

  DECL_SOURCE_LOCATION (decl) = location;

  DECL_EXTERNAL (decl) = imported;
  TREE_STATIC (decl) = isglobal;
  TREE_PUBLIC (decl) = exported || imported;

  gcc_assert ((istemporary == 0) || (istemporary == 1));

  /* The variable was not declared by GCC, but by the front end.  */
  DECL_ARTIFICIAL (decl) = istemporary;
  /* If istemporary then we don't want debug info for it.  */
  DECL_IGNORED_P (decl) = istemporary;
  /* If istemporary we don't want even the fancy names of those printed in
     -fdump-final-insns= dumps.  */
  DECL_NAMELESS (decl) = istemporary;

  /* Make the variable writable.  */
  TREE_READONLY (decl) = 0;

  DECL_CONTEXT (decl) = scope;

  if (initial)
    DECL_INITIAL (decl) = initial;

  m2block_pushDecl (decl);

  if (DECL_SIZE (decl) == 0)
    error ("storage size of %qD has not been resolved", decl);

  if ((TREE_PUBLIC (decl) == 0) && DECL_EXTERNAL (decl))
    internal_error ("inconsistent because %qs",
		    "PUBLIC_DECL(decl) == 0 && DECL_EXTERNAL(decl) == 1");

  m2block_addDeclExpr (build_stmt (location, DECL_EXPR, decl));

  return decl;
}

/* DeclareKnownConstant - given a constant, value, of, type, create a
   constant in the GCC symbol table.  Note that the name of the
   constant is not used as _all_ constants are declared in the global
   scope.  The front end deals with scoping rules - here we declare
   all constants with no names in the global scope.  This allows
   M2SubExp and constant folding routines the liberty of operating
   with quadruples which all assume constants can always be
   referenced.  */

tree
m2decl_DeclareKnownConstant (location_t location, tree type, tree value)
{
  tree id = make_node (IDENTIFIER_NODE); /* Ignore the name of the constant. */
  tree decl;

  m2assert_AssertLocation (location);
  m2expr_ConstantExpressionWarning (value);
  type = m2tree_skip_type_decl (type);
  layout_type (type);

  decl = build_decl (location, CONST_DECL, id, type);

  DECL_INITIAL (decl) = value;
  TREE_TYPE (decl) = type;

  decl = m2block_global_constant (decl);

  return decl;
}

/* BuildParameterDeclaration - creates and returns one parameter
   from, name, and, type.  It appends this parameter to the internal
   param_type_list.  */

tree
m2decl_BuildParameterDeclaration (location_t location, char *name, tree type,
                                  bool isreference)
{
  tree parm_decl;

  m2assert_AssertLocation (location);
  ASSERT_BOOL (isreference);
  type = m2tree_skip_type_decl (type);
  layout_type (type);
  if (isreference)
    type = build_reference_type (type);

  if (name == NULL)
    parm_decl = build_decl (location, PARM_DECL, NULL, type);
  else
    parm_decl = build_decl (location, PARM_DECL, get_identifier (name), type);
  DECL_ARG_TYPE (parm_decl) = type;
  if (isreference)
    TREE_READONLY (parm_decl) = TRUE;

  param_list = chainon (parm_decl, param_list);
  layout_type (type);
  param_type_list = tree_cons (NULL_TREE, type, param_type_list);
  return parm_decl;
}

/* BuildStartFunctionDeclaration - initializes global variables ready
   for building a function.  */

void
m2decl_BuildStartFunctionDeclaration (bool uses_varargs)
{
  if (uses_varargs)
    param_type_list = NULL_TREE;
  else
    param_type_list = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
  param_list = NULL_TREE; /* Ready for when we define a function.  */
}

/* BuildEndFunctionDeclaration - build a function which will return a
   value of returntype.  The arguments have been created by
   BuildParameterDeclaration.  */

tree
m2decl_BuildEndFunctionDeclaration (location_t location_begin,
                                    location_t location_end, const char *name,
                                    tree returntype, bool isexternal,
                                    bool isnested, bool ispublic, bool isnoreturn)
{
  tree fntype;
  tree fndecl;

  m2assert_AssertLocation (location_begin);
  m2assert_AssertLocation (location_end);
  ASSERT_BOOL (isexternal);
  ASSERT_BOOL (isnested);
  ASSERT_BOOL (ispublic);
  returntype = m2tree_skip_type_decl (returntype);
  /* The function type depends on the return type and type of args,
     both of which we have created in BuildParameterDeclaration */
  if (returntype == NULL_TREE)
    returntype = void_type_node;
  else if (TREE_CODE (returntype) == FUNCTION_TYPE)
    returntype = ptr_type_node;

  fntype = build_function_type (returntype, param_type_list);
  fndecl = build_decl (location_begin, FUNCTION_DECL, get_identifier (name),
                       fntype);

  if (isexternal)
    ASSERT_CONDITION (ispublic);

  DECL_EXTERNAL (fndecl) = isexternal;
  TREE_PUBLIC (fndecl) = ispublic;
  TREE_STATIC (fndecl) = (!isexternal);
  DECL_ARGUMENTS (fndecl) = param_list;
  DECL_RESULT (fndecl)
      = build_decl (location_end, RESULT_DECL, NULL_TREE, returntype);
  DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;
  TREE_TYPE (fndecl) = fntype;
  TREE_THIS_VOLATILE (fndecl) = isnoreturn;

  DECL_SOURCE_LOCATION (fndecl) = location_begin;

  /* Prevent the optimizer from removing it if it is public.  */
  if (TREE_PUBLIC (fndecl))
    gm2_mark_addressable (fndecl);

  m2block_pushDecl (fndecl);

  rest_of_decl_compilation (fndecl, 1, 0);
  param_list
      = NULL_TREE; /* Ready for the next time we call/define a function.  */
  return fndecl;
}

/* BuildModuleCtor creates the per module constructor used as part of
   the dynamic linking scaffold.  */

void
m2decl_BuildModuleCtor (tree module_ctor)
{
  decl_init_priority_insert (module_ctor, DEFAULT_INIT_PRIORITY);
}

/* DeclareModuleCtor configures the function to be used as a ctor.  */

tree
m2decl_DeclareModuleCtor (tree decl)
{
  /* Declare module_ctor ().  */
  TREE_PUBLIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_VISIBILITY (decl) = VISIBILITY_DEFAULT;
  DECL_VISIBILITY_SPECIFIED (decl) = 1;
  DECL_STATIC_CONSTRUCTOR (decl) = 1;
  return decl;
}

/* DetermineSizeOfConstant - given, str, and, base, fill in needsLong
   and needsUnsigned appropriately.  */

bool
m2decl_DetermineSizeOfConstant (location_t location,
				const char *str, unsigned int base,
                                bool *needsLong, bool *needsUnsigned,
				bool issueError)
{
  unsigned int ulow;
  int high;
  bool overflow = m2expr_interpret_m2_integer (location,
					       str, base, &ulow, &high,
					       needsLong, needsUnsigned);
  if (overflow && issueError)
    error_at (location, "constant %qs is too large", str);
  return overflow;
}

/* BuildConstLiteralNumber - returns a GCC TREE built from the
   string, str.  It assumes that, str, represents a legal number in
   Modula-2.  It always returns a positive value.  */

tree
m2decl_BuildConstLiteralNumber (location_t location, const char *str,
				unsigned int base, bool issueError)
{
  tree value, type;
  unsigned HOST_WIDE_INT low;
  HOST_WIDE_INT high;
  HOST_WIDE_INT ival[3];
  bool overflow = m2expr_interpret_integer (location, str, base, &low, &high);
  bool needLong, needUnsigned;

  ival[0] = low;
  ival[1] = high;
  ival[2] = 0;

  widest_int wval = widest_int::from_array (ival, 3);

  bool overflow_m2 = m2decl_DetermineSizeOfConstant (location, str, base,
						     &needLong, &needUnsigned,
						     issueError);
  if (needUnsigned && needLong)
    type = m2type_GetM2LongCardType ();
  else
    type = m2type_GetM2LongIntType ();

  value = wide_int_to_tree (type, wval);

  if (issueError && (overflow || overflow_m2 || m2expr_TreeOverflow (value)))
    error_at (location, "constant %qs is too large", str);

  return m2block_RememberConstant (value);
}

/* BuildCStringConstant - creates a string constant given a, string,
   and, length.  */

tree
m2decl_BuildCStringConstant (const char *string, int length)
{
  tree elem, index, type;

  /* +1 ensures that we always nul terminate our strings.  */
  elem = build_type_variant (char_type_node, 1, 0);
  index = build_index_type (build_int_cst (integer_type_node, length + 1));
  type = build_array_type (elem, index);
  return m2decl_BuildStringConstantType (length + 1, string, type);
}

/* BuildStringConstant - creates a string constant given a, string,
   and, length.  */

tree
m2decl_BuildStringConstant (const char *string, int length)
{
  tree elem, index, type;

  elem = build_type_variant (char_type_node, 1, 0);
  index = build_index_type (build_int_cst (integer_type_node, length));
  type = build_array_type (elem, index);
  return m2decl_BuildStringConstantType (length, string, type);
  // maybe_wrap_with_location
}


tree
m2decl_BuildPtrToTypeString (location_t location, const char *string, tree type)
{
  if ((string == NULL) || (strlen (string) == 0))
    return m2convert_BuildConvert (location, type,
				   m2decl_BuildIntegerConstant (0),
				   FALSE);
  return build_string_literal (strlen (string), string);
}


/* BuildIntegerConstant - return a tree containing the integer value.  */

tree
m2decl_BuildIntegerConstant (int value)
{
  switch (value)
    {

    case 0:
      return integer_zero_node;
    case 1:
      return integer_one_node;

    default:
      return m2block_RememberConstant (
          build_int_cst (integer_type_node, value));
    }
}

/* BuildStringConstantType - builds a string constant with a type.  */

tree
m2decl_BuildStringConstantType (int length, const char *string, tree type)
{
  tree id = build_string (length, string);

  TREE_TYPE (id) = type;
  TREE_CONSTANT (id) = TRUE;
  TREE_READONLY (id) = TRUE;
  TREE_STATIC (id) = TRUE;

  return m2block_RememberConstant (id);
}

/* GetBitsPerWord - returns the number of bits in a WORD.  */

int
m2decl_GetBitsPerWord (void)
{
  return BITS_PER_WORD;
}

/* GetBitsPerInt - returns the number of bits in a INTEGER.  */

int
m2decl_GetBitsPerInt (void)
{
  return INT_TYPE_SIZE;
}

/* GetBitsPerBitset - returns the number of bits in a BITSET.  */

int
m2decl_GetBitsPerBitset (void)
{
  return SET_WORD_SIZE;
}

/* GetBitsPerUnit - returns the number of bits in a UNIT.  */

int
m2decl_GetBitsPerUnit (void)
{
  return BITS_PER_UNIT;
}

/* m2decl_GetDeclContext - returns the DECL_CONTEXT of tree, t.  */

tree
m2decl_GetDeclContext (tree t)
{
  return DECL_CONTEXT (t);
}

#include "gt-m2-m2decl.h"
