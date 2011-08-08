/* Definitions for MIPS systems using GNU userspace and n32/64 abi.
   Copyright 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010, 2011
   Free Software Foundation, Inc.

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

/* Force the default endianness and ABI flags onto the command line
   in order to make the other specs easier to write.  */
#undef DRIVER_SELF_SPECS
#define DRIVER_SELF_SPECS \
  BASE_DRIVER_SELF_SPECS, \
  LINUX_DRIVER_SELF_SPECS \
  " %{!EB:%{!EL:%(endian_spec)}}" \
  " %{!mabi=*: -" MULTILIB_ABI_DEFAULT "}"

#undef LIB_SPEC
#define LIB_SPEC "\
%{pthread:-lpthread} \
%{shared:-lc} \
%{!shared: \
  %{profile:-lc_p} %{!profile:-lc}}"

#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{mips4} \
%{shared} \
 %(endian_spec) \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      %{mabi=n32: -dynamic-linker " GNU_USER_DYNAMIC_LINKERN32 "} \
      %{mabi=64: -dynamic-linker " GNU_USER_DYNAMIC_LINKER64 "} \
      %{mabi=32: -dynamic-linker " GNU_USER_DYNAMIC_LINKER32 "}} \
    %{static:-static}} \
%{mabi=n32:-m" GNU_USER_LINK_EMULATIONN32 "} \
%{mabi=64:-m" GNU_USER_LINK_EMULATION64 "} \
%{mabi=32:-m" GNU_USER_LINK_EMULATION32 "}"

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX (TARGET_OLDABI ? "$" : ".")

/* GNU/Linux doesn't use the same floating-point format that IRIX uses
   for long double.  There's no need to override this here, since
   ieee_quad_format is the default, but let's put this here to make
   sure nobody thinks we just forgot to set it to something else.  */
#define MIPS_TFMODE_FORMAT mips_quad_format
