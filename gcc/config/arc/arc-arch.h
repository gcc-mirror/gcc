/* Definitions of types that are used to store ARC architecture and
   device information.
   Copyright (C) 2016-2018 Free Software Foundation, Inc.
   Contributed by Claudiu Zissulescu (claziss@synopsys.com)

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

#ifndef GCC_ARC_ARCH_H
#define GCC_ARC_ARCH_H

#ifndef IN_LIBGCC2
/* Architecture selection types.  */

enum cpu_flags
  {
#define ARC_OPT(NAME, CODE, MASK, DOC)	    NAME = CODE,
#define ARC_OPTX(NAME, CODE, VAR, VAL, DOC0, DOC1) NAME = CODE,
#include "arc-options.def"
#undef ARC_OPT
#undef ARC_OPTX
    FL_END
  };


/* ARC architecture variants.  */

enum base_architecture
  {
    BASE_ARCH_NONE,
#define ARC_ARCH(NAME, ARCH, FLAGS, DFLAGS)  BASE_ARCH_##ARCH,
#include "arc-arches.def"
#undef ARC_ARCH
    BASE_ARCH_END
  };

/* Architecture specific propoerties.  */

typedef struct
{
  /* Architecture name.  */
  const char *const name;

  /* Architecture class.  */
  enum base_architecture arch_id;

  /* All allowed flags for this architecture.  */
  const unsigned long long flags;

  /* Default flags for this architecture.  It is a subset of
     FLAGS.  */
  const unsigned long long dflags;
} arc_arch_t;

/* Tune variants.  Needs to match the attr_tune enum.  */

enum arc_tune_attr
  {
    ARC_TUNE_NONE,
    ARC_TUNE_ARC600,
    ARC_TUNE_ARC700_4_2_STD,
    ARC_TUNE_ARC700_4_2_XMAC,
    ARC_TUNE_CORE_3,
    ARC_TUNE_ARCHS4X,
    ARC_TUNE_ARCHS4XD,
    ARC_TUNE_ARCHS4XD_SLOW
  };

/* Extra options for a processor template to hold any CPU specific
   information which is not cover in arc-arches.def.  Such example is
   the width of LP_COUNT register, or the number of register
   banks.  */

enum arc_extras
{
  HAS_NONE,
  HAS_LPCOUNT_16
};

/* CPU specific properties.  */

typedef struct
{
  /* CPU name.  */
  const char *const name;

  /* Architecture class.  */
  const arc_arch_t *arch_info;

  /* Specific processor type.  */
  enum processor_type processor;

  /* Specific flags.  */
  const unsigned long long flags;

  /* Extra value.  */
  enum arc_extras extra;

  /* Tune value.  */
  enum arc_tune_attr tune;

} arc_cpu_t;

const arc_arch_t arc_arch_types[] =
  {
    {"none", BASE_ARCH_NONE, 0, 0},
#define ARC_ARCH(NAME, ARCH, FLAGS, DFLAGS)	\
    {NAME, BASE_ARCH_##ARCH, FLAGS, DFLAGS},
#include "arc-arches.def"
#undef ARC_ARCH
    {NULL, BASE_ARCH_END, 0, 0}
  };

const arc_cpu_t arc_cpu_types[] =
  {
    {"none", NULL, PROCESSOR_NONE, 0, HAS_NONE, ARC_TUNE_NONE},
#define ARC_CPU(NAME, ARCH, FLAGS, EXTRA, TUNE)				\
    {#NAME, &arc_arch_types [BASE_ARCH_##ARCH], PROCESSOR_##NAME, FLAGS, HAS_##EXTRA, ARC_TUNE_##TUNE },
#include "arc-cpus.def"
#undef ARC_CPU
    {NULL, NULL, PROCESSOR_NONE, 0, HAS_NONE, ARC_TUNE_NONE}
  };

/* Currently selected cpu type.  */
extern const arc_cpu_t *arc_selected_cpu;

#endif
#endif /* GCC_ARC_ARCH_H */
