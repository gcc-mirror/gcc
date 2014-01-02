/* Copyright (C) 2011-2014 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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

/* Definitions for option handling for AArch64.  */

#ifndef GCC_AARCH64_OPTS_H
#define GCC_AARCH64_OPTS_H

/* The various cores that implement AArch64.  */
enum aarch64_processor
{
#define AARCH64_CORE(NAME, INTERNAL_IDENT, IDENT, ARCH, FLAGS, COSTS) \
  INTERNAL_IDENT,
#include "aarch64-cores.def"
#undef AARCH64_CORE
  /* Used to indicate that no processor has been specified.  */
  generic,
  /* Used to mark the end of the processor table.  */
  aarch64_none
};

/* TLS types.  */
enum aarch64_tls_type {
  TLS_TRADITIONAL,
  TLS_DESCRIPTORS
};

/* The code model defines the address generation strategy.
   Most have a PIC and non-PIC variant.  */
enum aarch64_code_model {
  /* Static code and data fit within a 1MB region.
     Not fully implemented, mostly treated as SMALL.  */
  AARCH64_CMODEL_TINY,
  /* Static code, data and GOT/PLT fit within a 1MB region.
     Not fully implemented, mostly treated as SMALL_PIC.  */
  AARCH64_CMODEL_TINY_PIC,
  /* Static code and data fit within a 4GB region.
     The default non-PIC code model.  */
  AARCH64_CMODEL_SMALL,
  /* Static code, data and GOT/PLT fit within a 4GB region.
     The default PIC code model.  */
  AARCH64_CMODEL_SMALL_PIC,
  /* No assumptions about addresses of code and data.
     The PIC variant is not yet implemented.  */
  AARCH64_CMODEL_LARGE
};

#endif
