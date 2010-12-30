/* Definitions of target machine for GCC, for any machine running Solaris 2
   using the GNU linker.

   Copyright (C) 2002, 2010 Free Software Foundation, Inc.

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

/* Undefine this so that attribute((init_priority)) works.  */
#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP

#undef SUPPORTS_INIT_PRIORITY
#define SUPPORTS_INIT_PRIORITY 1

/* GNU ld needs --export-dynamic to implement -rdynamic.  */
#undef RDYNAMIC_SPEC
#define RDYNAMIC_SPEC "--export-dynamic"

/* Solaris 11 build 135+ implements dl_iterate_phdr.  */
#if defined(HAVE_LD_EH_FRAME_HDR) && defined(TARGET_DL_ITERATE_PHDR)
#define LINK_EH_SPEC "%{!static:--eh-frame-hdr} "
#endif /* HAVE_LD_EH_FRAME && TARGET_DL_ITERATE_PHDR */
