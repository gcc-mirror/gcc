/* Processor and arch enumerations for C-SKY targets.
   Copyright (C) 2018-2025 Free Software Foundation, Inc.
   Contributed by C-SKY Microsystems and Mentor Graphics.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


#ifndef CSKY_OPTS_H
#define CSKY_OPTS_H


/* The various CSKY cores.  */
enum csky_processor_type
{
#undef CSKY_CORE
#define CSKY_CORE(NAME, INTERNAL_IDENT, IDENT, ARCH, ISA) \
  TARGET_CPU_##INTERNAL_IDENT,
#include "csky_cores.def"
#undef CSKY_CORE
  /* Used to indicate that no processor has been specified.  */
  TARGET_CPU_csky_none
};
#define CSKY_TARGET_CORE_GET(name) TARGET_CPU_ ## name

/* The various CSKY architectures.  */
enum csky_base_architecture
{
#undef CSKY_ARCH
#define CSKY_ARCH(NAME, CORE_IDENT, ARCH, ISA) \
  CSKY_BASE_ARCH_##ARCH,
#include "csky_cores.def"
#undef CSKY_ARCH
  CSKY_BASE_ARCH_NONE
};
#define CSKY_TARGET_ARCH_GET(name) CSKY_BASE_ARCH_ ## name

/* The various CSKY FPUs.  */
enum csky_fpu_type
{
#undef CSKY_FPU
#define CSKY_FPU(NAME, CNAME, ISA) TARGET_FPU_##CNAME,
#include "csky_cores.def"
  TARGET_FPU_auto
#undef CSKY_FPU
};
#define CSKY_TARGET_FPU_GET(name) TARGET_FPU_ ## name

enum float_abi_type
{
  CSKY_FLOAT_ABI_SOFT,
  CSKY_FLOAT_ABI_SOFTFP,
  CSKY_FLOAT_ABI_HARD
};


#endif /* CSKY_OPTS_H */
