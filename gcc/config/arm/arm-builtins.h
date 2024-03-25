/* Declarations for determining resolver for a given builtin.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_ARM_BUILTINS_H
#define GCC_ARM_BUILTINS_H

enum resolver_ident {
    arm_cde_resolver,
    arm_mve_resolver,
    arm_no_resolver
};
enum resolver_ident arm_describe_resolver (tree);
unsigned arm_cde_end_args (tree);

#define ENTRY(E, M, Q, S, T, G) E,
enum arm_simd_type
{
#include "arm-simd-builtin-types.def"
  __TYPE_FINAL
};
#undef ENTRY

enum arm_type_qualifiers
{
  /* T foo.  */
  qualifier_none = 0x0,
  /* unsigned T foo.  */
  qualifier_unsigned = 0x1, /* 1 << 0  */
  /* const T foo.  */
  qualifier_const = 0x2, /* 1 << 1  */
  /* T *foo.  */
  qualifier_pointer = 0x4, /* 1 << 2  */
  /* const T * foo.  */
  qualifier_const_pointer = 0x6,
  /* Used when expanding arguments if an operand could
     be an immediate.  */
  qualifier_immediate = 0x8, /* 1 << 3  */
  qualifier_unsigned_immediate = 0x9,
  qualifier_maybe_immediate = 0x10, /* 1 << 4  */
  /* void foo (...).  */
  qualifier_void = 0x20, /* 1 << 5  */
  /* Some patterns may have internal operands, this qualifier is an
     instruction to the initialisation code to skip this operand.  */
  qualifier_internal = 0x40, /* 1 << 6  */
  /* Some builtins should use the T_*mode* encoded in a simd_builtin_datum
     rather than using the type of the operand.  */
  qualifier_map_mode = 0x80, /* 1 << 7  */
  /* qualifier_pointer | qualifier_map_mode  */
  qualifier_pointer_map_mode = 0x84,
  /* qualifier_const_pointer | qualifier_map_mode  */
  qualifier_const_pointer_map_mode = 0x86,
  /* Polynomial types.  */
  qualifier_poly = 0x100,
  /* Lane indices - must be within range of previous argument = a vector.  */
  qualifier_lane_index = 0x200,
  /* Lane indices for single lane structure loads and stores.  */
  qualifier_struct_load_store_lane_index = 0x400,
  /* A void pointer.  */
  qualifier_void_pointer = 0x800,
  /* A const void pointer.  */
  qualifier_const_void_pointer = 0x802,
  /* Lane indices selected in pairs - must be within range of previous
     argument = a vector.  */
  qualifier_lane_pair_index = 0x1000,
  /* Lane indices selected in quadtuplets - must be within range of previous
     argument = a vector.  */
  qualifier_lane_quadtup_index = 0x2000,
  /* MVE vector predicates.  */
  qualifier_predicate = 0x4000
};

struct arm_simd_type_info
{
  enum arm_simd_type type;

  /* Internal type name.  */
  const char *name;

  /* Internal type name(mangled).  The mangled names conform to the
     AAPCS (see "Procedure Call Standard for the ARM Architecture",
     Appendix A).  To qualify for emission with the mangled names defined in
     that document, a vector type must not only be of the correct mode but also
     be of the correct internal Neon vector type (e.g. __simd64_int8_t);
     these types are registered by arm_init_simd_builtin_types ().  In other
     words, vector types defined in other ways e.g. via vector_size attribute
     will get default mangled names.  */
  const char *mangle;

  /* Internal type.  */
  tree itype;

  /* Element type.  */
  tree eltype;

  /* Machine mode the internal type maps to.  */
  machine_mode mode;

  /* Qualifiers.  */
  enum arm_type_qualifiers q;
};

extern struct arm_simd_type_info arm_simd_types[];

#endif /* GCC_ARM_BUILTINS_H */
