/* Definitions for MIPS running Linux-based GNU systems with ELF format
   using n32/64 abi.
   Copyright 2002, 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This sets the post-install default ABI to n32.  This must NOT be
   kept in sync with the default ABI in gcc/config.gcc; it's actually
   meant to override that.  However, for correct behavior at build
   time, we also need t-linux64 to get the build-time specs in line
   with the setting in config.gcc.  */
#define DRIVER_DEFAULT_ABI_SELF_SPEC "%{!mabi=*:-mabi=n32}"
#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "driver_default_abi_self_spec", DRIVER_DEFAULT_ABI_SELF_SPEC },
#define DRIVER_SELF_SPECS \
"%{!EB:%{!EL:%(endian_spec)}}", \
"%{mabi-fake-default:%{!mabi=*:-mabi=32}}", \
"%(driver_default_abi_self_spec)", \
"%{!mips*:%{!march=*:%{mabi=32:-mips1}%{mabi=n32|mabi=64:-mips3}}}"

#undef SUBTARGET_TARGET_SWITCHES
#define SUBTARGET_TARGET_SWITCHES \
  { "abi-fake-default", 0, N_("Same as -mabi=32, just trickier") },

#undef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC "\
%{!fno-PIC:%{!fno-pic:-KPIC}} \
%{fno-PIC:-non_shared} %{fno-pic:-non_shared} \
%{mabi=64:-64} %{mabi=n32:-n32}"

#undef LIB_SPEC
#define LIB_SPEC "\
%{shared: -lc} \
%{!static: \
 %{mabi=n32:-rpath-link %R/lib32:%R/usr/lib32} \
 %{mabi=64:-rpath-link %R/lib64:%R/usr/lib64} \
 %{mabi=32:-rpath-link %R/lib:%R/usr/lib}} \
%{!shared: %{pthread:-lpthread} \
  %{profile:-lc_p} %{!profile: -lc}}"

#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{mips4} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
 %(endian_spec) \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker: \
	  %{mabi=n32: -dynamic-linker /lib32/ld.so.1} \
	  %{mabi=64: -dynamic-linker /lib64/ld.so.1} \
	  %{mabi=32: -dynamic-linker /lib/ld.so.1}}} \
      %{static:-static}}} \
%{mabi=n32:-melf32%{EB:b}%{EL:l}tsmipn32} \
%{mabi=64:-melf64%{EB:b}%{EL:l}tsmip} \
%{mabi=32:-melf32%{EB:b}%{EL:l}tsmip}"

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX ((mips_abi == ABI_32 || mips_abi == ABI_O64) \
			    ? "$" : ".")

/* The size in bytes of a DWARF field indicating an offset or length
   relative to a debug info section, specified to be 4 bytes in the DWARF-2
   specification.  The SGI/MIPS ABI defines it to be the same as PTR_SIZE.  */
#define DWARF_OFFSET_SIZE PTR_SIZE

/* GNU/Linux doesn't use the same floating-point format that IRIX uses
   for long double.  There's no need to override this here, since
   ieee_quad_format is the default, but let's put this here to make
   sure nobody thinks we just forgot to set it to something else.  */
#define MIPS_TFMODE_FORMAT mips_quad_format
