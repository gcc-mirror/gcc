/* Builtins' description for AArch64 SIMD architecture.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
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
#ifndef GCC_AARCH64_BUILTINS_H
#define GCC_AARCH64_BUILTINS_H

enum aarch64_type_qualifiers
{
  /* T foo.  */
  qualifier_none = 0x0,
  /* unsigned T foo.  */
  qualifier_unsigned = 0x1, /* 1 << 0  */
  /* const T foo.  */
  qualifier_const = 0x2, /* 1 << 1  */
  /* T *foo.  */
  qualifier_pointer = 0x4, /* 1 << 2  */
  /* Used when expanding arguments if an operand could
     be an immediate.  */
  qualifier_immediate = 0x8, /* 1 << 3  */
  qualifier_maybe_immediate = 0x10, /* 1 << 4  */
  /* void foo (...).  */
  qualifier_void = 0x20, /* 1 << 5  */
  /* 1 << 6 is now unused */
  /* Some builtins should use the T_*mode* encoded in a simd_builtin_datum
     rather than using the type of the operand.  */
  qualifier_map_mode = 0x80, /* 1 << 7  */
  /* qualifier_pointer | qualifier_map_mode  */
  qualifier_pointer_map_mode = 0x84,
  /* qualifier_const | qualifier_pointer | qualifier_map_mode  */
  qualifier_const_pointer_map_mode = 0x86,
  /* Polynomial types.  */
  qualifier_poly = 0x100,
  /* Lane indices - must be in range, and flipped for bigendian.  */
  qualifier_lane_index = 0x200,
  /* Lane indices for single lane structure loads and stores.  */
  qualifier_struct_load_store_lane_index = 0x400,
  /* Lane indices selected in pairs. - must be in range, and flipped for
     bigendian.  */
  qualifier_lane_pair_index = 0x800,
  /* Lane indices selected in quadtuplets. - must be in range, and flipped for
     bigendian.  */
  qualifier_lane_quadtup_index = 0x1000,
  /* Modal FP types.  */
  qualifier_modal_float = 0x2000,
};

#define ENTRY(E, M, Q, G) E,
enum aarch64_simd_type
{
#include "aarch64-simd-builtin-types.def"
  ARM_NEON_H_TYPES_LAST
};
#undef ENTRY

struct GTY(()) aarch64_simd_type_info
{
  enum aarch64_simd_type type;

  /* Internal type name.  */
  const char *name;

  /* Internal type name(mangled).  The mangled names conform to the
     AAPCS64 (see "Procedure Call Standard for the ARM 64-bit Architecture",
     Appendix A).  To qualify for emission with the mangled names defined in
     that document, a vector type must not only be of the correct mode but also
     be of the correct internal AdvSIMD vector type (e.g. __Int8x8_t); these
     types are registered by aarch64_init_simd_builtin_types ().  In other
     words, vector types defined in other ways e.g. via vector_size attribute
     will get default mangled names.  */
  const char *mangle;

  /* Internal type.  */
  tree itype;

  /* Element type.  */
  tree eltype;

  /* Machine mode the internal type maps to.  */
  enum machine_mode mode;

  /* Qualifiers.  */
  enum aarch64_type_qualifiers q;
};

extern aarch64_simd_type_info aarch64_simd_types[];

#endif