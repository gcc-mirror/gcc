/* Copyright (C) 1999 Free Software Foundation, Inc. */

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

/* Undefine the following which were defined in elf.h.  Thise will cause the mips-vxworks
   port to continue to use collect2 for constructors/destructors.  This entire file may
   be removed when .ctor/.dtor section support is desired. */

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
