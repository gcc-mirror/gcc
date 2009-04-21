/* Definitions of target machine for GCC, for bi-arch SPARC
   running Solaris 2 using the GNU linker.

Copyright (C) 2002, 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#undef LINK_ARCH32_SPEC
#define LINK_ARCH32_SPEC \
  LINK_ARCH32_SPEC_BASE "%{!static: -rpath-link %R/usr/lib}"

#undef LINK_ARCH64_SPEC
#define LINK_ARCH64_SPEC \
  LINK_ARCH64_SPEC_BASE "%{!static: -rpath-link %R/usr/lib/sparcv9}"

#undef LINK_ARCH_SPEC
#if DISABLE_MULTILIB
#if DEFAULT_ARCH32_P
#define LINK_ARCH_SPEC "\
%{m32:-m elf32_sparc %(link_arch32)} \
%{m64:%edoes not support multilib} \
%{!m32:%{!m64:%(link_arch_default)}} \
"
#else
#define LINK_ARCH_SPEC "\
%{m32:%edoes not support multilib} \
%{m64:-m elf64_sparc %(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}} \
"
#endif
#else
#define LINK_ARCH_SPEC "\
%{m32:-m elf32_sparc %(link_arch32)} \
%{m64:-m elf64_sparc %(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}} \
"
#endif

