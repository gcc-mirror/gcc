/* Definitions of target machine GNU compiler.  IA-64 version.
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Steve Ellcey <sje@cup.hp.com> and
                  Reva Cuthbertson <reva@cup.hp.com>

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

/* This macro is a C statement to print on `stderr' a string describing the
   particular machine description choice.  */

#define TARGET_VERSION fprintf (stderr, " (IA-64) HP-UX");

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "\
  -D__IA64__ -D__hpux -D__hpux__ -Dhpux -Dunix \
  -D__BIG_ENDIAN__ -D_LONGLONG \
  -Asystem=hpux -Asystem=posix -Asystem=unix \
  -D_UINT128_T"

/* -D__fpreg=long double is needed to compensate for the lack of __fpreg
   which is a primitive type in HP C but does not exist in GNU C.  Same
   for __float80 and __float128.  These types appear in HP-UX header
   files and so must have some definition.  */

#undef CPP_SPEC
#define CPP_SPEC "\
  %{mcpu=itanium:-D__itanium__} \
  %{mlp64:-D__LP64__ -D_LP64 -D__LONG_MAX__=9223372036854775807L} \
  %{!ansi:%{!std=c*:%{!std=i*: -D_HPUX_SOURCE -D__STDC_EXT__}}} \
  -D__fpreg=long\\ double \
  -D__float80=long\\ double \
  -D__float128=long\\ double"

#undef  ASM_EXTRA_SPEC
#define ASM_EXTRA_SPEC "%{milp32:-milp32} %{mlp64:-mlp64}"

#undef ENDFILE_SPEC

#undef STARTFILE_SPEC
#ifdef CROSS_COMPILE
#define STARTFILE_SPEC "%{!shared:crt0%O%s}"
#else
#define STARTFILE_SPEC "/usr/ccs/lib/hpux64/crt0%O"
#endif

#undef LINK_SPEC
#define LINK_SPEC "\
  +Accept TypeMismatch \
  %{shared:-b} \
  %{!shared: \
    -u main \
    %{!static: \
      %{rdynamic:-export-dynamic}} \
      %{static:-static}}"

#undef  LIB_SPEC
#define LIB_SPEC "%{!shared:%{!symbolic:-lc}}"

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
  { "ilp32",    MASK_ILP32,     "Generate ILP32 code" }, \
  { "lp64",    -MASK_ILP32,     "Generate LP64 code" },

/* A C expression whose value is zero if pointers that need to be extended
   from being `POINTER_SIZE' bits wide to `Pmode' are sign-extended and
   greater then zero if they are zero-extended and less then zero if the
   ptr_extend instruction should be used.  */

#define POINTERS_EXTEND_UNSIGNED -1

#define JMP_BUF_SIZE  (8 * 76)

#undef CONST_SECTION_ASM_OP
#define CONST_SECTION_ASM_OP    "\t.section\t.rodata,\t\"a\",\t\"progbits\""

#undef BITS_BIG_ENDIAN
#define BITS_BIG_ENDIAN 1

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_DWARF2_ASM | MASK_BIG_ENDIAN)

/* This needs to be set to force structure arguments with a single
   field to be treated as structures and not as the type of their
   field.  Without this a structure with a single char will be
   returned just like a char variable and that is wrong on HP-UX
   IA64.  TARGET_STRUCT_ARG_REG_LITTLE_ENDIAN triggers the special
   structure handling, this macro simply ensures that single field
   structures are always treated like structures.  */

#define MEMBER_TYPE_FORCES_BLK(FIELD) 1

/* Override the setting of FUNCTION_ARG_REG_LITTLE_ENDIAN in
   defaults.h.  Setting this to true means that we are not passing
   structures in registers in the "normal" big-endian way.  See
   See section 8.5 of the "Itanium Software Conventions and Runtime
   Architecture", specifically Table 8-1 and the explanation of Byte 0
   alignment and LSB alignment and a description of how structures
   are passed.  */

#define FUNCTION_ARG_REG_LITTLE_ENDIAN 1

#undef FUNCTION_ARG_PADDING
#define FUNCTION_ARG_PADDING(MODE, TYPE) \
	ia64_hpux_function_arg_padding ((MODE), (TYPE))

#undef PAD_VARARGS_DOWN
#define PAD_VARARGS_DOWN (!AGGREGATE_TYPE_P (type))
