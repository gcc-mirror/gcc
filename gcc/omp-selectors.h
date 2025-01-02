/* Data structures for OpenMP context selectors.  This is in a separate file
   from omp-general.h so that it may also be used in the Fortran parser
   without reference to tree data structures.

   Copyright (C) 2023-2025 Free Software Foundation, Inc.

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


#ifndef GCC_OMP_SELECTORS_H
#define GCC_OMP_SELECTORS_H

/* Trait set selector keywords.  */
enum omp_tss_code {
  OMP_TRAIT_SET_CONSTRUCT,
  OMP_TRAIT_SET_DEVICE,
  OMP_TRAIT_SET_TARGET_DEVICE,
  OMP_TRAIT_SET_IMPLEMENTATION,
  OMP_TRAIT_SET_USER,
  OMP_TRAIT_SET_LAST,
  OMP_TRAIT_SET_INVALID = -1
};

/* Trait selector keywords.  */
enum omp_ts_code {
  OMP_TRAIT_DEVICE_KIND,
  OMP_TRAIT_DEVICE_ISA,
  OMP_TRAIT_DEVICE_ARCH,
  OMP_TRAIT_DEVICE_NUM,
  OMP_TRAIT_IMPLEMENTATION_VENDOR,
  OMP_TRAIT_IMPLEMENTATION_EXTENSION,
  OMP_TRAIT_IMPLEMENTATION_ADMO,
  OMP_TRAIT_IMPLEMENTATION_REQUIRES,
  OMP_TRAIT_IMPLEMENTATION_UNIFIED_ADDRESS,
  OMP_TRAIT_IMPLEMENTATION_UNIFIED_SHARED_MEMORY,
  OMP_TRAIT_IMPLEMENTATION_SELF_MAPS,
  OMP_TRAIT_IMPLEMENTATION_DYNAMIC_ALLOCATORS,
  OMP_TRAIT_IMPLEMENTATION_REVERSE_OFFLOAD,
  OMP_TRAIT_USER_CONDITION,
  OMP_TRAIT_CONSTRUCT_TARGET,
  OMP_TRAIT_CONSTRUCT_TEAMS,
  OMP_TRAIT_CONSTRUCT_PARALLEL,
  OMP_TRAIT_CONSTRUCT_FOR,
  OMP_TRAIT_CONSTRUCT_SIMD,
  OMP_TRAIT_CONSTRUCT_DISPATCH,
  OMP_TRAIT_LAST,
  OMP_TRAIT_INVALID = -1
};

/* All trait property forms.  */
enum omp_tp_type {
  OMP_TRAIT_PROPERTY_NONE,
  OMP_TRAIT_PROPERTY_ID,
  OMP_TRAIT_PROPERTY_NAME_LIST,
  OMP_TRAIT_PROPERTY_DEV_NUM_EXPR,
  OMP_TRAIT_PROPERTY_BOOL_EXPR,
  OMP_TRAIT_PROPERTY_CLAUSE_LIST,
  OMP_TRAIT_PROPERTY_EXTENSION
};

/* Map trait set selector name keywords onto strings.  */
extern const char *omp_tss_map [];

/* Map trait selector keywords onto strings, allowed contexts, and
   allowed property names for OMP_TRAIT_PROPERTY_NAME_LIST and
   OMP_TRAIT_PROPERTY_ID properties.  If valid_properties is null,
   it means that any required checking has to be done explicitly
   somewhere instead of being driven by the table.  Otherwise it's a
   null-terminated array of strings.  */
struct omp_ts_info {
  const char *name;
  unsigned int tss_mask;
  enum omp_tp_type tp_type;
  bool allow_score;
  const char * const *valid_properties;
};
extern struct omp_ts_info omp_ts_map[];

extern enum omp_tss_code omp_lookup_tss_code (const char *);
extern enum omp_ts_code omp_lookup_ts_code (enum omp_tss_code, const char *);

#endif /* GCC_OMP_SELECTORS_H */
