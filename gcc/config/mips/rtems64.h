/* Definitions for rtems targeting a MIPS ORION using ecoff.
   Copyright (C) 1996, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Joel Sherrill (joel@OARcorp.com).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Specify predefined symbols in preprocessor.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmips -DMIPSEB -DR4000 -D_mips -D_MIPSEB -D_R4000 \
   -Drtems -D__rtems__ -Asystem(rtems)"

/* Generate calls to memcpy, memcmp and memset.  */
#ifndef TARGET_MEM_FUNCTIONS
#define TARGET_MEM_FUNCTIONS
#endif

/* Undefine the following which were defined in elf64.h.  This will cause the rtems64
   port to continue to use collect2 for constructors/destructors.  These may be removed
   when .ctor/.dtor section support is desired. */

#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_sdata, in_rdata

#undef INVOKE__main
#undef NAME__MAIN
#undef SYMBOL__MAIN

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS                                         \
  SECTION_FUNCTION_TEMPLATE(sdata_section, in_sdata, SDATA_SECTION_ASM_OP) \
  SECTION_FUNCTION_TEMPLATE(rdata_section, in_rdata, RDATA_SECTION_ASM_OP)

#undef ASM_OUTPUT_CONSTRUCTOR
#undef ASM_OUTPUT_DESTRUCTOR

#undef CTOR_LIST_BEGIN
#undef CTOR_LIST_END
#undef DTOR_LIST_BEGIN
#undef DTOR_LIST_END

#undef STARTFILE_SPEC
#undef ENDFILE_SPEC

/*  End of undefines to turn off .ctor/.dtor section support */

/* Get machine-independent configuration parameters for RTEMS.  */
#include <rtems.h>
