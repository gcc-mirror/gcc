/* ACLE support for Arm MVE
   Copyright (C) 2021-2023 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "fold-const.h"
#include "langhooks.h"
#include "stringpool.h"
#include "attribs.h"
#include "diagnostic.h"
#include "arm-protos.h"
#include "arm-builtins.h"
#include "arm-mve-builtins.h"

namespace arm_mve {

/* Static information about each single-predicate or single-vector
   ACLE type.  */
struct vector_type_info
{
  /* The name of the type as declared by arm_mve.h.  */
  const char *acle_name;

  /* Whether the type requires a floating point abi.  */
  const bool requires_float;
};

/* Flag indicating whether the arm MVE types have been handled.  */
static bool handle_arm_mve_types_p;

/* Information about each single-predicate or single-vector type.  */
static CONSTEXPR const vector_type_info vector_types[] = {
#define DEF_MVE_TYPE(ACLE_NAME, SCALAR_TYPE) \
  { #ACLE_NAME, REQUIRES_FLOAT },
#include "arm-mve-builtins.def"
#undef DEF_MVE_TYPE
};

/* The scalar type associated with each vector type.  */
GTY(()) tree scalar_types[NUM_VECTOR_TYPES];

/* The single-predicate and single-vector types, with their built-in
   "__simd128_..._t" name.  Allow an index of NUM_VECTOR_TYPES, which always
   yields a null tree.  */
static GTY(()) tree abi_vector_types[NUM_VECTOR_TYPES + 1];

/* Same, but with the arm_mve.h names.  */
GTY(()) tree acle_vector_types[3][NUM_VECTOR_TYPES + 1];

/* Return the MVE abi type with element of type TYPE.  */
static tree
arm_mve_type_for_scalar_type (tree eltype)
{
  for (unsigned int i = 0; i < __TYPE_FINAL; ++i)
      if (arm_simd_types[i].eltype == eltype
	  && GET_MODE_SIZE (arm_simd_types[i].mode) == 16)
	return arm_simd_types[i].itype;

  gcc_unreachable ();
}

/* Register the built-in MVE ABI vector types, such as uint32x4_t.  */
static void
register_builtin_types ()
{
#define DEF_MVE_TYPE(ACLE_NAME, SCALAR_TYPE) \
  scalar_types[VECTOR_TYPE_ ## ACLE_NAME] = SCALAR_TYPE;
#include "arm-mve-builtins.def"
#undef DEF_MVE_TYPE
  for (unsigned int i = 0; i < NUM_VECTOR_TYPES; ++i)
    {
      if (vector_types[i].requires_float && !TARGET_HAVE_MVE_FLOAT)
	continue;
      tree eltype = scalar_types[i];
      tree vectype;
      if (eltype == boolean_type_node)
	{
	  vectype = get_typenode_from_name (UINT16_TYPE);
	  gcc_assert (GET_MODE_SIZE (TYPE_MODE (vectype)) == 2);
	}
      else
	{
	  vectype = arm_mve_type_for_scalar_type (eltype);
	  gcc_assert (VECTOR_MODE_P (TYPE_MODE (vectype))
		      && GET_MODE_SIZE (TYPE_MODE (vectype)) == 16);
	}
      abi_vector_types[i] = vectype;
    }
}

/* Register vector type TYPE under its arm_mve.h name.  */
static void
register_vector_type (vector_type_index type)
{
  if (vector_types[type].requires_float && !TARGET_HAVE_MVE_FLOAT)
    return;
  tree vectype = abi_vector_types[type];
  tree id = get_identifier (vector_types[type].acle_name);
  tree decl = build_decl (input_location, TYPE_DECL, id, vectype);
  decl = lang_hooks.decls.pushdecl (decl);

  /* Record the new ACLE type if pushdecl succeeded without error.  Use
     the ABI type otherwise, so that the type we record at least has the
     right form, even if it doesn't have the right name.  This should give
     better error recovery behavior than installing error_mark_node or
     installing an incorrect type.  */
  if (decl
      && TREE_CODE (decl) == TYPE_DECL
      && TREE_TYPE (decl) != error_mark_node
      && TYPE_MAIN_VARIANT (TREE_TYPE (decl)) == vectype)
    vectype = TREE_TYPE (decl);
  acle_vector_types[0][type] = vectype;
}

/* Register tuple type TYPE with NUM_VECTORS arity under its
   arm_mve_types.h name.  */
static void
register_builtin_tuple_types (vector_type_index type)
{
  const vector_type_info* info = &vector_types[type];
  if (scalar_types[type] == boolean_type_node
      || (info->requires_float && !TARGET_HAVE_MVE_FLOAT))
    return;
  const char *vector_type_name = info->acle_name;
  char buffer[sizeof ("float32x4x2_t")];
  for (unsigned int num_vectors = 2; num_vectors <= 4; num_vectors += 2)
    {
      snprintf (buffer, sizeof (buffer), "%.*sx%d_t",
		(int) strlen (vector_type_name) - 2, vector_type_name,
		num_vectors);

      tree vectype = acle_vector_types[0][type];
      tree arrtype = build_array_type_nelts (vectype, num_vectors);
      gcc_assert (TYPE_MODE_RAW (arrtype) == TYPE_MODE (arrtype));
      tree field = build_decl (input_location, FIELD_DECL,
			       get_identifier ("val"), arrtype);

      tree t = lang_hooks.types.simulate_record_decl (input_location, buffer,
						      make_array_slice (&field,
									1));
      gcc_assert (TYPE_MODE_RAW (t) == TYPE_MODE (t));
      acle_vector_types[num_vectors >> 1][type] = TREE_TYPE (t);
    }
}

/* Implement #pragma GCC arm "arm_mve_types.h".  */
void
handle_arm_mve_types_h ()
{
  if (handle_arm_mve_types_p)
    {
      error ("duplicate definition of %qs", "arm_mve_types.h");
      return;
    }
  handle_arm_mve_types_p = true;
  if (!TARGET_HAVE_MVE)
    {
      error ("this definition requires the MVE ISA extension");
      return;
    }
  register_builtin_types ();
  for (unsigned int type_i = 0; type_i < NUM_VECTOR_TYPES; ++type_i)
    {
      vector_type_index type = vector_type_index (type_i);
      register_vector_type (type);
      if (type_i != VECTOR_TYPE_mve_pred16_t)
	register_builtin_tuple_types (type);
    }
}

} /* end namespace arm_mve */

using namespace arm_mve;

#include "gt-arm-mve-builtins.h"
