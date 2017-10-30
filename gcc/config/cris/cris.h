/* Definitions for GCC.  Part of the machine description for CRIS.
   Copyright (C) 1998-2017 Free Software Foundation, Inc.
   Contributed by Axis Communications.  Written by Hans-Peter Nilsson.

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

/* After the first "Node:" comment comes all preprocessor directives and
   attached declarations described in the info files, the "Using and
   Porting GCC" manual (uapgcc), in the same order as found in the "Target
   macros" section in the gcc-2.9x CVS edition of 2000-03-17.  FIXME: Not
   really, but needs an update anyway.

   There is no generic copy-of-uapgcc comment, you'll have to see uapgcc
   for that.  If applicable, there is a CRIS-specific comment.  The order
   of macro definitions follow the order in the manual.  Every section in
   the manual (node in the info pages) has an introductory `Node:
   <subchapter>' comment.  If no macros are defined for a section, only
   the section-comment is present.  */

/* Note that other header files (e.g. config/elfos.h, config/linux.h,
   and config/cris/linux.h) are responsible for lots of settings not
   repeated below.  This file contains general CRIS definitions
   and definitions for the cris-*-elf subtarget.  */

/* We don't want to use gcc_assert for everything, as that can be
   compiled out.  */
#define CRIS_ASSERT(x) \
 do { if (!(x)) internal_error ("CRIS-port assertion failed: " #x); } while (0)

/* Replacement for REG_P since it does not match SUBREGs.  Happens for
   testcase Axis-20000320 with gcc-2.9x.  */
#define REG_S_P(x) \
 (REG_P (x) || (GET_CODE (x) == SUBREG && REG_P (XEXP (x, 0))))

/* Last register in main register bank r0..r15.  */
#define CRIS_LAST_GENERAL_REGISTER 15

/* Descriptions of registers used for arguments.  */
#define CRIS_FIRST_ARG_REG 10
#define CRIS_MAX_ARGS_IN_REGS 4

/* See also *_REGNUM constants in cris.md.  */

/* Most of the time, we need the index into the register-names array.
   When passing debug-info, we need the real hardware register number.  */
#define CRIS_CANONICAL_SRP_REGNUM (16 + 11)
#define CRIS_CANONICAL_MOF_REGNUM (16 + 7)
/* We have CCR in all models including v10, but that's 16 bits, so let's
   prefer the DCCR number, which is a DMA pointer in pre-v8, so we'll
   never clash with it for GCC purposes.  */
#define CRIS_CANONICAL_CC0_REGNUM (16 + 13)

/* When generating PIC, these suffixes are added to the names of non-local
   functions when being output.  Contrary to other ports, we have offsets
   relative to the GOT, not the PC.  We might implement PC-relative PLT
   semantics later for the general case; they are used in some cases right
   now, such as MI thunks.  */
#define CRIS_GOTPLT_SUFFIX ":GOTPLT"
#define CRIS_PLT_GOTOFFSET_SUFFIX ":PLTG"
#define CRIS_PLT_PCOFFSET_SUFFIX ":PLT"

#define CRIS_FUNCTION_ARG_SIZE(MODE, TYPE)	\
  ((MODE) != BLKmode ? GET_MODE_SIZE (MODE)	\
   : (unsigned) int_size_in_bytes (TYPE))

/* Which CPU version this is.  The parsed and adjusted cris_cpu_str.  */
extern int cris_cpu_version;


/* Node: Driver */

/* Also provide canonical vN definitions when user specifies an alias.  */

#define CPP_SPEC \
 "%{mtune=*:-D__tune_%* %{mtune=v*:-D__CRIS_arch_tune=%*}\
   %{mtune=etrax4:-D__tune_v3 -D__CRIS_arch_tune=3}\
   %{mtune=etrax100:-D__tune_v8 -D__CRIS_arch_tune=8}\
   %{mtune=svinto:-D__tune_v8 -D__CRIS_arch_tune=8}\
   %{mtune=etrax100lx:-D__tune_v10 -D__CRIS_arch_tune=10}\
   %{mtune=ng:-D__tune_v10 -D__CRIS_arch_tune=10}}\
  %{mcpu=*:-D__arch_%* %{mcpu=v*:-D__CRIS_arch_version=%*}\
   %{mcpu=etrax4:-D__arch_v3 -D__CRIS_arch_version=3}\
   %{mcpu=etrax100:-D__arch_v8 -D__CRIS_arch_version=8}\
   %{mcpu=svinto:-D__arch_v8 -D__CRIS_arch_version=8}\
   %{mcpu=etrax100lx:-D__arch_v10 -D__CRIS_arch_version=10}\
   %{mcpu=ng:-D__arch_v10 -D__CRIS_arch_version=10}}\
  %{march=*:-D__arch_%* %{march=v*:-D__CRIS_arch_version=%*}\
   %{march=etrax4:-D__arch_v3 -D__CRIS_arch_version=3}\
   %{march=etrax100:-D__arch_v8 -D__CRIS_arch_version=8}\
   %{march=svinto:-D__arch_v8 -D__CRIS_arch_version=8}\
   %{march=etrax100lx:-D__arch_v10 -D__CRIS_arch_version=10}\
   %{march=ng:-D__arch_v10 -D__CRIS_arch_version=10}}\
  %{metrax100:-D__arch__v8 -D__CRIS_arch_version=8}\
  %{metrax4:-D__arch__v3 -D__CRIS_arch_version=3}\
  %(cpp_subtarget)"

/* For the cris-*-elf subtarget.  */

#define CRIS_DEFAULT_TUNE "10"
#define CRIS_ARCH_CPP_DEFAULT
#define CRIS_DEFAULT_ASM_ARCH_OPTION ""

#ifdef TARGET_CPU_DEFAULT
#if TARGET_CPU_DEFAULT != 32 && TARGET_CPU_DEFAULT != 10
 #error "Due to '()'; e.g. '#define TARGET_CPU_DEFAULT (10)', stringize TARGET_CPU_DEFAULT isn't useful: update manually."
#endif

#if TARGET_CPU_DEFAULT == 32
#undef CRIS_DEFAULT_TUNE
#define CRIS_DEFAULT_TUNE "32"
/* To enable use of "generic" cris-axis-elf binutils, always pass the
   architecture option to GAS.  (We don't do this for non-v32.)  */
#undef CRIS_DEFAULT_ASM_ARCH_OPTION
#define CRIS_DEFAULT_ASM_ARCH_OPTION "--march=v32"
#endif

#undef CRIS_ARCH_CPP_DEFAULT
#define CRIS_ARCH_CPP_DEFAULT \
 "%{!march=*:\
   %{!metrax*:\
    %{!mcpu=*:\
     %{!mtune=*:-D__tune_v" CRIS_DEFAULT_TUNE "}\
     -D__arch_v" CRIS_DEFAULT_TUNE \
   " -D__CRIS_arch_version=" CRIS_DEFAULT_TUNE "}}}"
#endif

#define CRIS_CPP_SUBTARGET_SPEC \
 "%{mbest-lib-options:\
   %{!moverride-best-lib-options:\
   %{!march=*:%{!metrax*:%{!mcpu=*:\
      -D__tune_v" CRIS_DEFAULT_TUNE \
    " -D__CRIS_arch_tune=" CRIS_DEFAULT_TUNE "}}}}}"\
 CRIS_ARCH_CPP_DEFAULT

/* Override previous definitions (../linux.h).  */
#undef CC1_SPEC
#define CC1_SPEC \
 "%{metrax4:-march=v3}\
  %{metrax100:-march=v8}\
  %{march=*:-march=%*}\
  %{mcpu=*:-mcpu=%*}\
  %(cc1_subtarget)"

/* For the cris-*-elf subtarget.  */
#define CRIS_CC1_SUBTARGET_SPEC \
 "-melf\
  %{mbest-lib-options:\
   %{!moverride-best-lib-options:\
   %{!march=*:%{!mcpu=*:-mtune=v" CRIS_DEFAULT_TUNE\
       " -D__CRIS_arch_tune=" CRIS_DEFAULT_TUNE "}}\
    %{!finhibit-size-directive:\
      %{!fno-function-sections: -ffunction-sections}\
      %{!fno-data-sections: -fdata-sections}}}}"

/* This adds to CC1_SPEC.  */
#define CC1PLUS_SPEC ""

#ifdef HAVE_AS_NO_MUL_BUG_ABORT_OPTION
#define MAYBE_AS_NO_MUL_BUG_ABORT \
 "%{mno-mul-bug-workaround:-no-mul-bug-abort} "
#else
#define MAYBE_AS_NO_MUL_BUG_ABORT
#endif

/* Override previous definitions (../linux.h).  */
#undef ASM_SPEC
#define ASM_SPEC \
 MAYBE_AS_NO_MUL_BUG_ABORT \
 "%(asm_subtarget)\
 %{march=*:%{mcpu=*:%edo not specify both -march=... and -mcpu=...}}\
 %{march=v0|mcpu=v0|march=v3|mcpu=v3|march=v8|mcpu=v8:--march=v0_v10}\
 %{march=v10|mcpu=v10:--march=v10}\
 %{march=v32|mcpu=v32:--march=v32}"

/* For the cris-*-elf subtarget.  */
#define CRIS_ASM_SUBTARGET_SPEC \
 "--em=criself %{!march=*:%{!mcpu=*:" CRIS_DEFAULT_ASM_ARCH_OPTION "}}"

/* FIXME: We should propagate the -melf option to make the criself
   "emulation" unless a linker script is provided (-T*), but I don't know
   how to do that if either of -Ttext, -Tdata or -Tbss is given but no
   linker script, as is usually the case.  Leave it to the user for the
   time being.  */
#undef LINK_SPEC
#define LINK_SPEC \
 "%{v:--verbose}\
  %(link_subtarget)"

/* For the cris-*-elf subtarget.  */
#define CRIS_LINK_SUBTARGET_SPEC \
 "-mcriself\
  %{sim2:%{!T*:-Tdata 0x4000000 -Tbss 0x8000000}}\
  %{!r:%{O2|O3: --gc-sections}}"

/* Which library to get.  The simulator uses a different library for
   the low-level syscalls (implementing the Linux syscall ABI instead
   of direct-iron accesses).  Default everything with the stub "nosys"
   library.  */
/* Override previous definitions (linux.h).  */
#undef LIB_SPEC
#define LIB_SPEC \
 "%{sim*:--start-group -lc -lsyslinux --end-group}\
  %{!sim*:%{g*:-lg}\
    %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} -lbsp}\
  -lnosys"

/* Linker startfile options; crt0 flavors.
   We need to remove any previous definition (elfos.h).  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
 "%{sim*:crt1.o%s}%{!sim*:crt0.o%s}\
  crti.o%s crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

#define EXTRA_SPECS				\
  {"cpp_subtarget", CRIS_CPP_SUBTARGET_SPEC},	\
  {"cc1_subtarget", CRIS_CC1_SUBTARGET_SPEC},	\
  {"asm_subtarget", CRIS_ASM_SUBTARGET_SPEC},	\
  {"link_subtarget", CRIS_LINK_SUBTARGET_SPEC},	\
  CRIS_SUBTARGET_EXTRA_SPECS

#define CRIS_SUBTARGET_EXTRA_SPECS


/* Node: Run-time Target */

#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("cris");		\
      builtin_define_std ("CRIS");		\
      builtin_define_std ("GNU_CRIS");		\
      builtin_define ("__CRIS_ABI_version=2");	\
      builtin_assert ("cpu=cris");		\
      builtin_assert ("machine=cris");		\
    }						\
  while (0)

/* Previously controlled by target_flags.  Note that this is *not* set
   for -melinux.  */
#define TARGET_LINUX 0

/* For the cris-*-elf subtarget.  */
#define CRIS_SUBTARGET_DEFAULT 0

#define CRIS_CPU_BASE 0
#define CRIS_CPU_ETRAX4 3	/* Just lz added.  */
#define CRIS_CPU_SVINTO 8	/* Added swap, jsrc & Co., 32-bit accesses.  */
#define CRIS_CPU_NG 10		/* Added mul[su].  */
#define CRIS_CPU_V32 32		/* Major changes.  */

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT CRIS_CPU_BASE
#endif

/* Default target_flags if no switches specified.
   The alignment-by-32 is to make builtin atomic support for v10 and v32
   work for *-elf for types without specified alignment (like plain
   "int").  See top comment in sync.md.  */
#ifndef TARGET_DEFAULT
# if TARGET_CPU_DEFAULT == 32
#  define TARGET_DEFAULT \
 (MASK_STACK_ALIGN \
  + MASK_CONST_ALIGN + MASK_DATA_ALIGN \
  + MASK_ALIGN_BY_32 \
  + MASK_PROLOGUE_EPILOGUE)
# elif TARGET_CPU_DEFAULT == 10
#  define TARGET_DEFAULT \
 (MASK_SIDE_EFFECT_PREFIXES + MASK_STACK_ALIGN \
  + MASK_CONST_ALIGN + MASK_DATA_ALIGN \
  + MASK_ALIGN_BY_32 \
  + MASK_PROLOGUE_EPILOGUE + MASK_MUL_BUG)
# else  /* 0 */
#  define TARGET_DEFAULT \
 (MASK_SIDE_EFFECT_PREFIXES + MASK_STACK_ALIGN \
  + MASK_CONST_ALIGN + MASK_DATA_ALIGN \
  + MASK_PROLOGUE_EPILOGUE + MASK_MUL_BUG)
# endif
#endif

/* Local, providing a default for cris_cpu_version.  */
#define CRIS_DEFAULT_CPU_VERSION TARGET_CPU_DEFAULT

#define TARGET_HAS_MUL_INSNS (cris_cpu_version >= CRIS_CPU_NG)
#define TARGET_HAS_LZ (cris_cpu_version >= CRIS_CPU_ETRAX4)
#define TARGET_HAS_BREAK (cris_cpu_version >= CRIS_CPU_ETRAX4)
#define TARGET_HAS_SWAP (cris_cpu_version >= CRIS_CPU_SVINTO)
#define TARGET_V32 (cris_cpu_version >= CRIS_CPU_V32)

/* The "break" instruction was introduced with ETRAX 4.  */
#define TARGET_TRAP_USING_BREAK8 \
 (cris_trap_using_break8 == 2 ? TARGET_HAS_BREAK : cris_trap_using_break8)

/* Call library functions by default for GNU/Linux.  */
#define TARGET_ATOMICS_MAY_CALL_LIBFUNCS		\
 (cris_atomics_calling_libfunc == 2			\
  ? TARGET_LINUX : cris_atomics_calling_libfunc)

/* The < v10 atomics turn off interrupts, so they don't need alignment.
   Incidentally, by default alignment is off there causing variables to
   be default unaligned all over, so we'd have to make support
   libraries use a proper atomic type (instead of "int"), one we'd
   specify as aligned.  */
#define TARGET_TRAP_UNALIGNED_ATOMIC		\
 (cris_trap_unaligned_atomic == 2		\
  ? (TARGET_V32 || cris_cpu_version == 10)	\
  : cris_trap_unaligned_atomic)

/* Node: Storage Layout */

#define BITS_BIG_ENDIAN 0

#define BYTES_BIG_ENDIAN 0

/* WORDS_BIG_ENDIAN is not defined in the hardware, but for consistency,
   we use little-endianness, and we may also be able to use
   post-increment on DImode indirect.  */
#define WORDS_BIG_ENDIAN 0

#define UNITS_PER_WORD 4

#define CRIS_PROMOTED_MODE(MODE, UNSIGNEDP, TYPE) \
 (GET_MODE_CLASS (MODE) == MODE_INT && GET_MODE_SIZE (MODE) < 4) \
  ? SImode : MODE

/* We will be using prototype promotion, so they will be 32 bit.  */
#define PARM_BOUNDARY 32

/* Stack boundary is guided by -mstack-align, -mno-stack-align,
   -malign.
   Old comment: (2.1: still valid in 2.7.2?)
    Note that to make this macro affect the alignment of stack
   locals, a fix was required, and special precautions when handling
   the stack pointer in various other macros (TARGET_ASM_FUNCTION_PROLOGUE
   et al) were required.  See file "function.c".  If you would just define
   this macro, it would only affect the builtin alloca and variable
   local data (non-ANSI, non-K&R, Gnu C extension).  */
#define STACK_BOUNDARY \
 (TARGET_STACK_ALIGN ? (TARGET_ALIGN_BY_32 ? 32 : 16) : 8)

#define FUNCTION_BOUNDARY 16

/* Do not change BIGGEST_ALIGNMENT (when optimizing), as it will affect
   strange places, at least in 2.1.  */
#define BIGGEST_ALIGNMENT 8

/* If -m16bit,	-m16-bit, -malign or -mdata-align,
   align everything to 16 bit.  */
#define DATA_ALIGNMENT(TYPE, BASIC_ALIGN)			\
 (TARGET_DATA_ALIGN						\
  ? (TARGET_ALIGN_BY_32						\
     ? (BASIC_ALIGN < 32 ? 32 : BASIC_ALIGN)			\
     : (BASIC_ALIGN < 16 ? 16 : BASIC_ALIGN)) : BASIC_ALIGN)

/* FIXME: Define LOCAL_ALIGNMENT for word and dword or arrays and
   structures (if -mstack-align=), and check that it is good.  */

#define EMPTY_FIELD_BOUNDARY 8

#define STRUCTURE_SIZE_BOUNDARY 8

#define STRICT_ALIGNMENT 0

/* Remove any previous definition (elfos.h).
   ??? If it wasn't for all the other stuff that affects layout of
   structures and bit-fields, this could presumably cause incompatibility
   with other GNU/Linux ports (i.e. elfos.h users).  */
#undef PCC_BITFIELD_TYPE_MATTERS

/* This is only used for non-scalars.  Strange stuff happens to structs
   (FIXME: What?) if we use anything larger than largest actually used
   datum size, so lets make it 32.  The type "long long" will still work
   as usual.  We can still have DImode insns, but they will only be used
   for scalar data (i.e. long long).  */
#define MAX_FIXED_MODE_SIZE 32


/* Node: Type Layout */

/* Note that DOUBLE_TYPE_SIZE is not defined anymore, since the default
   value gives a 64-bit double, which is what we now use.  */

/* For compatibility and historical reasons, a char should be signed.  */
#define DEFAULT_SIGNED_CHAR 1

/* Note that WCHAR_TYPE_SIZE is used in cexp.y,
   where TARGET_SHORT is not available.  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32


/* Node: Register Basics */

/*  We count all 16 non-special registers, SRP, a faked argument
    pointer register, MOF and CCR/DCCR.  */
#define FIRST_PSEUDO_REGISTER (16 + 1 + 1 + 1 + 1)

/* For CRIS, these are r15 (pc) and r14 (sp). Register r8 is used as a
   frame-pointer, but is not fixed.  SRP is not included in general
   registers and will not be used automatically.  All other special
   registers are fixed at the moment.  The faked argument pointer register
   is fixed too.  */
#define FIXED_REGISTERS \
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1}

/* Register r9 is used for structure-address, r10-r13 for parameters,
   r10- for return values.  */
#define CALL_USED_REGISTERS \
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1}

/* Node: Allocation Order */

/* We need this on CRIS, because call-used regs should be used first,
   (so we don't need to push).  Else start using registers from r0 and up.
    This preference is mainly because if we put call-used-regs from r0
   and up, then we can't use movem to push the rest, (which have to be
   saved if we use them, and movem has to start with r0).
   Change here if you change which registers to use as call registers.

   The actual need to explicitly prefer call-used registers improved the
   situation a lot for 2.1, but might not actually be needed anymore.
   Still, this order reflects what GCC should find out by itself, so it
   probably does not hurt.

   Order of preference: Call-used-regs first, then r0 and up, last fp &
   sp & pc as fillers.
   Call-used regs in opposite order, so they will cause less conflict if
   a function has few args (<= 3) and it wants a scratch reg.
    Use struct-return address first, since very few functions use
   structure return values so it is likely to be available.  */
#define REG_ALLOC_ORDER \
 {9, 13, 12, 11, 10, 0, 1, 2, 3, 4, 5, 6, 7, 8, 14, 15, 17, 16, 18, 19}

/* Use MOF and ACR.  Prefer ACR before any other register.  Prefer MOF
   then SRP after saved registers.  The *after* is because they're only
   useful for storage, not for things being computed, which is
   apparently more common.  */
#define REG_ALLOC_ORDER_V32 \
 {15, 9, 13, 12, 11, 10, 0, 1, 2, 3, 4, 5, 6, 7, 8, 17, 16, 14, 18, 19}


/* Node: Leaf Functions */
/* (no definitions) */

/* Node: Stack Registers */
/* (no definitions) */


/* Node: Register Classes */

/* We need a separate register class to handle register allocation for
   ACR, since it can't be used for post-increment.

   It's not obvious, but having subunions of all movable-between
   register classes does really help register allocation (pre-IRA
   comment).  */
enum reg_class
  {
    NO_REGS,
    ACR_REGS, MOF_REGS, SRP_REGS, CC0_REGS,
    MOF_SRP_REGS, SPECIAL_REGS,
    SPEC_ACR_REGS, GENNONACR_REGS,
    SPEC_GENNONACR_REGS, GENERAL_REGS,
    ALL_REGS,
    LIM_REG_CLASSES
  };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES						\
  {"NO_REGS",							\
   "ACR_REGS", "MOF_REGS", "SRP_REGS", "CC0_REGS",		\
   "MOF_SRP_REGS", "SPECIAL_REGS",				\
   "SPEC_ACR_REGS", "GENNONACR_REGS", "SPEC_GENNONACR_REGS",	\
   "GENERAL_REGS", "ALL_REGS"}

#define CRIS_SPECIAL_REGS_CONTENTS					\
 ((1 << CRIS_SRP_REGNUM) | (1 << CRIS_MOF_REGNUM) | (1 << CRIS_CC0_REGNUM))

/* Count in the faked argument register in GENERAL_REGS.  Keep out SRP.  */
#define REG_CLASS_CONTENTS			\
  {						\
   {0},						\
   {1 << CRIS_ACR_REGNUM},			\
   {1 << CRIS_MOF_REGNUM},			\
   {1 << CRIS_SRP_REGNUM},			\
   {1 << CRIS_CC0_REGNUM},			\
   {(1 << CRIS_MOF_REGNUM)			\
    | (1 << CRIS_SRP_REGNUM)},			\
   {CRIS_SPECIAL_REGS_CONTENTS},		\
   {CRIS_SPECIAL_REGS_CONTENTS			\
    | (1 << CRIS_ACR_REGNUM)},			\
   {(0xffff | (1 << CRIS_AP_REGNUM))		\
    & ~(1 << CRIS_ACR_REGNUM)},			\
   {(0xffff | (1 << CRIS_AP_REGNUM)		\
    | CRIS_SPECIAL_REGS_CONTENTS)		\
    & ~(1 << CRIS_ACR_REGNUM)},			\
   {0xffff | (1 << CRIS_AP_REGNUM)},		\
   {0xffff | (1 << CRIS_AP_REGNUM)		\
    | CRIS_SPECIAL_REGS_CONTENTS}		\
  }

#define REGNO_REG_CLASS(REGNO)			\
  ((REGNO) == CRIS_ACR_REGNUM ? ACR_REGS :	\
   (REGNO) == CRIS_MOF_REGNUM ? MOF_REGS :	\
   (REGNO) == CRIS_SRP_REGNUM ? SRP_REGS :	\
   (REGNO) == CRIS_CC0_REGNUM ? CC0_REGS :	\
   GENERAL_REGS)

#define BASE_REG_CLASS GENERAL_REGS

#define MODE_CODE_BASE_REG_CLASS(MODE, AS, OCODE, ICODE)	\
  ((OCODE) != POST_INC ? BASE_REG_CLASS : GENNONACR_REGS)

#define INDEX_REG_CLASS GENERAL_REGS

/* Since it uses reg_renumber, it is safe only once reg_renumber
   has been allocated, which happens in reginfo.c during register
   allocation.  */
#define REGNO_OK_FOR_BASE_P(REGNO)					\
 ((REGNO) <= CRIS_LAST_GENERAL_REGISTER					\
  || (REGNO) == ARG_POINTER_REGNUM					\
  || (unsigned) reg_renumber[REGNO] <= CRIS_LAST_GENERAL_REGISTER	\
  || (unsigned) reg_renumber[REGNO] == ARG_POINTER_REGNUM)

/* REGNO_OK_FOR_BASE_P seems to be obsolete wrt. this one, but not yet
   documented as such.  */
#define REGNO_MODE_CODE_OK_FOR_BASE_P(REGNO, MODE, AS, OCODE, ICODE)	\
 (REGNO_OK_FOR_BASE_P (REGNO)						\
  && ((OCODE) != POST_INC						\
      || !((REGNO) == CRIS_ACR_REGNUM					\
	   || (unsigned) reg_renumber[REGNO] == CRIS_ACR_REGNUM)))

/* See REGNO_OK_FOR_BASE_P.  */
#define REGNO_OK_FOR_INDEX_P(REGNO) REGNO_OK_FOR_BASE_P(REGNO)

/* We can't move special registers to and from memory in smaller than
   word_mode.  We also can't move between special registers.  Luckily,
   -1, as returned by true_regnum for non-sub/registers, is valid as a
   parameter to our REGNO_REG_CLASS, returning GENERAL_REGS, so we get
   the effect that any X that isn't a special-register is treated as
   a non-empty intersection with GENERAL_REGS.  */
#define SECONDARY_RELOAD_CLASS(CLASS, MODE, X)				\
 ((reg_class_subset_p (CLASS, SPECIAL_REGS)				\
   && ((GET_MODE_SIZE (MODE) < 4 && MEM_P (X))				\
       || !reg_classes_intersect_p (REGNO_REG_CLASS (true_regnum (X)),	\
				    GENERAL_REGS)))			\
   ? GENERAL_REGS : NO_REGS)

/* FIXME: Fix regrename.c; it should check validity of replacements,
   not just with a silly pass-specific macro.  We may miss some
   opportunities, but we must stop regrename from creating acr++.  */
#define HARD_REGNO_RENAME_OK(FROM, TO) ((TO) != CRIS_ACR_REGNUM)

/* For CRIS, this is always the size of MODE in words,
   since all registers are the same size.  To use omitted modes in
   patterns with reload constraints, you must say the widest size
   which is allowed for VOIDmode.
   FIXME:  Does that still apply for gcc-2.9x?  Keep poisoned until such
   patterns are added back.  News: 2001-03-16: Happens as early as the
   underscore-test.  */
#define CLASS_MAX_NREGS(CLASS, MODE)					\
 ((MODE) == VOIDmode							\
  ? 1 /* + cris_fatal ("CLASS_MAX_NREGS with VOIDmode")	*/		\
  : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))


/* Node: Frame Layout */

#define STACK_GROWS_DOWNWARD 1
#define FRAME_GROWS_DOWNWARD 1

#define FIRST_PARM_OFFSET(FNDECL) 0

#define RETURN_ADDR_RTX(COUNT, FRAMEADDR) \
 cris_return_addr_rtx (COUNT, FRAMEADDR)

#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, CRIS_SRP_REGNUM)

/* FIXME: Any __builtin_eh_return callers must not return anything and
   there must not be collisions with incoming parameters.  Luckily the
   number of __builtin_eh_return callers is limited.  For now return
   parameter registers in reverse order and hope for the best.  */
#define EH_RETURN_DATA_REGNO(N) \
  (IN_RANGE ((N), 0, 3) ? (CRIS_FIRST_ARG_REG + 3 - (N)) : INVALID_REGNUM)

/* Store the stack adjustment in the structure-return-address register.  */
#define CRIS_STACKADJ_REG CRIS_STRUCT_VALUE_REGNUM
#define EH_RETURN_STACKADJ_RTX gen_rtx_REG (SImode, CRIS_STACKADJ_REG)

#define EH_RETURN_HANDLER_RTX \
  cris_return_addr_rtx (0, NULL)

#define INIT_EXPANDERS cris_init_expanders ()

/* FIXME: Move this to right node (it's not documented properly yet).  */
#define DWARF_FRAME_RETURN_COLUMN DWARF_FRAME_REGNUM (CRIS_SRP_REGNUM)

/* FIXME: Move this to right node (it's not documented properly yet).
   FIXME: Check what alignment we can assume regarding
   TARGET_STACK_ALIGN and TARGET_ALIGN_BY_32.  */
#define DWARF_CIE_DATA_ALIGNMENT -1

/* If we would ever need an exact mapping between canonical register
   number and dwarf frame register, we would either need to include all
   registers in the gcc description (with some marked fixed of course), or
   an inverse mapping from dwarf register to gcc register.  There is one
   need in dwarf2out.c:expand_builtin_init_dwarf_reg_sizes.  Right now, I
   don't see that we need exact correspondence between DWARF *frame*
   registers and DBX_REGISTER_NUMBER, so map them onto GCC registers.  */
#define DWARF_FRAME_REGNUM(REG) (REG)

/* Node: Stack Checking */
/* (no definitions) FIXME: Check.  */

/* Node: Frame Registers */

#define STACK_POINTER_REGNUM CRIS_SP_REGNUM

/* Register used for frame pointer.  This is also the last of the saved
   registers, when a frame pointer is not used.  */
#define FRAME_POINTER_REGNUM CRIS_FP_REGNUM

/* Faked register, is always eliminated.  We need it to eliminate
   allocating stack slots for the return address and the frame pointer.  */
#define ARG_POINTER_REGNUM CRIS_AP_REGNUM

#define STATIC_CHAIN_REGNUM CRIS_STATIC_CHAIN_REGNUM


/* Node: Elimination */

#define ELIMINABLE_REGS				\
 {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
  {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},	\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
 (OFFSET) = cris_initial_elimination_offset (FROM, TO)


/* Node: Stack Arguments */

/* Since many parameters take up one register each in any case,
   defining TARGET_PROMOTE_PROTOTYPES that always returns true would
   seem like a good idea, but measurements indicate that a combination
   using PROMOTE_MODE is better.  */

#define ACCUMULATE_OUTGOING_ARGS 1


/* Node: Register Arguments */

/* Contrary to what you'd believe, defining FUNCTION_ARG_CALLEE_COPIES
   seems like a (small total) loss, at least for gcc-2.7.2 compiling and
   running gcc-2.1 (small win in size, small loss running -- 100.1%),
   and similarly for size for products (.1 .. .3% bloat, sometimes win).
   Due to the empirical likeliness of making slower code, it is not
   defined.  */

/* This no longer *needs* to be a structure; but keeping it as such should
   not hurt (and hacking the ABI is simpler).  */
#define CUMULATIVE_ARGS struct cum_args
struct cum_args {int regs;};

/* The regs member is an integer, the number of arguments got into
   registers so far.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
 ((CUM).regs = 0)

#define FUNCTION_ARG_REGNO_P(REGNO)			\
 ((REGNO) >= CRIS_FIRST_ARG_REG				\
  && (REGNO) < CRIS_FIRST_ARG_REG + (CRIS_MAX_ARGS_IN_REGS))


/* Node: Aggregate Return */

#define CRIS_STRUCT_VALUE_REGNUM ((CRIS_FIRST_ARG_REG) - 1)


/* Node: Caller Saves */
/* (no definitions) */

/* Node: Function entry */

/* See cris.c for TARGET_ASM_FUNCTION_PROLOGUE and
   TARGET_ASM_FUNCTION_EPILOGUE.  */

/* Node: Profiling */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
 error ("no FUNCTION_PROFILER for CRIS")

/* FIXME: Some of the undefined macros might be mandatory.  If so, fix
   documentation.  */


/* Node: Trampolines */

#define TRAMPOLINE_SIZE (TARGET_V32 ? 58 : 32)

/* CRIS wants instructions on word-boundary.  */
#define TRAMPOLINE_ALIGNMENT 16

/* Node: Library Calls */

/* If you change this, you have to check whatever libraries and systems
   that use it.  */
#define TARGET_EDOM 33


/* Node: Addressing Modes */

#define HAVE_POST_INCREMENT 1

#define CONSTANT_ADDRESS_P(X) \
  (CONSTANT_P (X) && cris_legitimate_address_p (QImode, X, false))

/* Must be a compile-time constant, so we go with the highest value
   among all CRIS variants.  */
#define MAX_REGS_PER_ADDRESS 2

/* Fix reloads known to cause suboptimal spilling.  */
#define LEGITIMIZE_RELOAD_ADDRESS(X, MODE, OPNUM, TYPE, INDL, WIN)	\
  do									\
    {									\
      if (cris_reload_address_legitimized (X, MODE, OPNUM, TYPE, INDL))	\
	goto WIN;							\
    }									\
  while (0)

/* The mode argument to cris_legitimate_constant_p isn't used, so just
   pass a cheap dummy.  N.B. we have to cast away const from the
   parameter rather than adjust the parameter, as it's type is mandated
   by the TARGET_LEGITIMATE_CONSTANT_P target hook interface.  */
#define CRIS_CONSTANT_P(X) \
  (CONSTANT_P (X) && cris_legitimate_constant_p (VOIDmode, CONST_CAST_RTX (X)))

/* Node: Condition Code */

#define NOTICE_UPDATE_CC(EXP, INSN) cris_notice_update_cc (EXP, INSN)

/* FIXME: Maybe define CANONICALIZE_COMPARISON later, when playing with
   optimizations.  It is needed; currently we do this with instruction
   patterns and NOTICE_UPDATE_CC.  */


/* Node: Costs */

/* Regardless of the presence of delay slots, the default value of 1 for
   BRANCH_COST is the best in the range (1, 2, 3), tested with gcc-2.7.2
   with testcases ipps and gcc, giving smallest and fastest code.  */

#define SLOW_BYTE_ACCESS 0

/* This is the threshold *below* which inline move sequences of
   word-length sizes will be emitted.  The "9" will translate to
   (9 - 1) * 4 = 32 bytes maximum moved, but using 16 instructions
   (8 instruction sequences) or less.  */
#define MOVE_RATIO(speed) 9


/* Node: Sections */

#define TEXT_SECTION_ASM_OP "\t.text"

#define DATA_SECTION_ASM_OP "\t.data"

/* The jump table is immediately connected to the preceding insn.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1


/* Node: PIC */

/* Helper type.  */

enum cris_symbol_type
  {
    cris_no_symbol = 0,
    cris_got_symbol = 1,
    cris_rel_symbol = 2,
    cris_got_symbol_needing_fixup = 3,
    cris_unspec = 7,
    cris_offsettable_symbol = 8
  };

#define PIC_OFFSET_TABLE_REGNUM (flag_pic ? CRIS_GOT_REGNUM : INVALID_REGNUM)

#define LEGITIMATE_PIC_OPERAND_P(X) cris_legitimate_pic_operand (X)


/* Node: File Framework */

/* We don't want an .ident for gcc.  To avoid that but still support
   #ident, we override TARGET_ASM_OUTPUT_IDENT and, since the gcc .ident
   is its only use besides front-end .ident directives, we return if
   the state if the cgraph is not PARSING.  */
#undef TARGET_ASM_OUTPUT_IDENT
#define TARGET_ASM_OUTPUT_IDENT cris_asm_output_ident

#define ASM_APP_ON "#APP\n"

#define ASM_APP_OFF "#NO_APP\n"


/* Node: Data Output */

#define IS_ASM_LOGICAL_LINE_SEPARATOR(C, STR) (C) == '@'

/* Node: Uninitialized Data */

/* Remember to round off odd values if we want data alignment,
   since we cannot do that with an .align directive.

   Using .comm causes the space not to be reserved in .bss, but by
   tricks with the symbol type.  Not good if other tools than binutils
   are used on the object files.  Since ".global ... .lcomm ..." works, we
   use that.  Use .._ALIGNED_COMMON, since gcc whines when we only have
   ..._COMMON, and we prefer to whine ourselves; BIGGEST_ALIGNMENT is not
   the one to check.  */
/* FIXME: I suspect a bug in gcc with alignment.  Do not warn until
   investigated; it mucks up the testsuite results.  */
#define CRIS_ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN, LOCAL) \
  do									\
    {									\
      int align_ = (ALIGN) / BITS_PER_UNIT;				\
      if (TARGET_DATA_ALIGN && TARGET_ALIGN_BY_32 && align_ < 4)	\
	align_ = 4;							\
      else if (TARGET_DATA_ALIGN && align_ < 2)				\
	align_ = 2;							\
      /* FIXME: Do we need this?  */					\
      else if (align_ < 1)						\
	align_ = 1;							\
									\
      if (LOCAL)							\
	{								\
	  fprintf ((FILE), "%s", LOCAL_ASM_OP);				\
	  assemble_name ((FILE), (NAME));				\
	  fprintf ((FILE), "\n");					\
	}								\
      fprintf ((FILE), "%s", COMMON_ASM_OP);				\
      assemble_name ((FILE), (NAME));					\
      fprintf ((FILE), ",%u,%u\n", (int)(SIZE), align_);		\
    }									\
  while (0)

#define ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN) \
 CRIS_ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN, 0)

#undef ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN) \
 CRIS_ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN, 1)

/* Node: Label Output */

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global "

#define SUPPORTS_WEAK 1

#define ASM_OUTPUT_SYMBOL_REF(STREAM, SYM) \
 cris_asm_output_symbol_ref (STREAM, SYM)

#define ASM_OUTPUT_LABEL_REF(STREAM, BUF) \
 cris_asm_output_label_ref (STREAM, BUF)

/* Remove any previous definition (elfos.h).  */
#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)	\
  sprintf (LABEL, "*%s%s%ld", LOCAL_LABEL_PREFIX, PREFIX, (long) NUM)

/* Node: Initialization */
/* (no definitions) */

/* Node: Macros for Initialization */
/* (no definitions) */

/* Node: Instruction Output */

#define REGISTER_NAMES					\
 {"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8",	\
  "r9", "r10", "r11", "r12", "r13", "sp", "acr", "srp", "mof", "faked_ap", "dccr"}

#define ADDITIONAL_REGISTER_NAMES \
 {{"r14", 14}, {"r15", 15}, {"pc", 15}}

/* Output an empty line to illustrate the presence of the delay slot.  */
#define DBR_OUTPUT_SEQEND(FILE) \
  fprintf (FILE, "\n")

#define LOCAL_LABEL_PREFIX "."

/* cppinit.c initializes a const array from this, so it must be constant,
   can't have it different based on options.  Luckily, the prefix is
   always allowed, so let's have it on all GCC-generated code.  Note that
   we have this verbatim everywhere in the back-end, not using %R or %s or
   such.  */
#define REGISTER_PREFIX "$"

/* Remove any previous definition (elfos.h).  */
/* We use -fno-leading-underscore to remove it, when necessary.  */
#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"

#define ASM_OUTPUT_REG_PUSH(FILE, REGNO)				\
  fprintf (FILE,							\
	   TARGET_V32							\
	   ? "\tsubq 4,$sp\n\tmove $%s,[$sp]\n" : "\tpush $%s\n",	\
	   reg_names[REGNO])

#define ASM_OUTPUT_REG_POP(FILE, REGNO) \
  fprintf (FILE, "\tmove [$sp+],$%s\n", reg_names[REGNO])


/* Node: Dispatch Tables */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)		\
  do									\
    {									\
      if (TARGET_V32)							\
       asm_fprintf (FILE, "\t.word %LL%d-.\n", VALUE);			\
      else								\
       asm_fprintf (FILE, "\t.word %LL%d-%LL%d\n", VALUE, REL);		\
    }									\
  while (0)

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  asm_fprintf (FILE, "\t.dword %LL%d\n", VALUE)

/* Defined to also emit an .align in elfos.h.  We don't want that.  */
#undef ASM_OUTPUT_CASE_LABEL

/* Since the "bound" insn loads the comparison value if the compared<
   value (register) is out of bounds (0..comparison value-1), we need
   to output another case to catch it.
   The way to find it is to look for the label_ref at the else-arm inside
   the expanded casesi core-insn.
   FIXME: Check this construct when changing to new version of gcc.  */
#define ASM_OUTPUT_CASE_END(STREAM, NUM, TABLE)				\
  cris_asm_output_case_end (STREAM, NUM, TABLE)


/* Node: Exception Region Output */
/* (no definitions) */
/* FIXME: Fill in with our own optimized layout.  */

/* Node: Alignment Output */

#define ASM_OUTPUT_ALIGN(FILE, LOG)  \
 fprintf (FILE, "\t.align %d\n", (LOG))


/* Node: All Debuggers */

#define DBX_REGISTER_NUMBER(REGNO)				\
 ((REGNO) == CRIS_SRP_REGNUM ? CRIS_CANONICAL_SRP_REGNUM :	\
  (REGNO) == CRIS_MOF_REGNUM ? CRIS_CANONICAL_MOF_REGNUM :	\
  (REGNO) == CRIS_CC0_REGNUM ? CRIS_CANONICAL_CC0_REGNUM :	\
 (REGNO))

/* FIXME: Investigate DEBUGGER_AUTO_OFFSET, DEBUGGER_ARG_OFFSET.  */


/* Node: DBX Options */

/* Is this correct? Check later.  */
#define DBX_NO_XREFS

#define DBX_CONTIN_LENGTH 0

/* FIXME: Is this needed when we have 0 DBX_CONTIN_LENGTH?  */
#define DBX_CONTIN_CHAR '?'


/* Node: DBX Hooks */
/* (no definitions) */

/* Node: File names and DBX */
/* (no definitions) */


/* Node: DWARF */
/* (no definitions) */

/* Node: Misc */

/* A combination of the bound (umin) insn together with a
   sign-extended add via the table to PC seems optimal.
   If the table overflows, the assembler will take care of it.
   Theoretically, in extreme cases (uncertain if they occur), an error
   will be emitted, so FIXME: Check how large case-tables are emitted,
   possible add an option to emit SImode case-tables.  */
#define CASE_VECTOR_MODE HImode

#define CASE_VECTOR_PC_RELATIVE 1

/* FIXME: Investigate CASE_VECTOR_SHORTEN_MODE to make sure HImode is not
   used when broken-.word could possibly fail (plus testcase).  */

/* This is the number of bytes that can be moved in one
   reasonably fast instruction sequence.  For CRIS, this is two
   instructions: mem => reg, reg => mem.  */
#define MOVE_MAX 4

/* Maybe SHIFT_COUNT_TRUNCATED is safe to define?  FIXME: Check later.  */

#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) ((VALUE) = 32, 1)
#define CTZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) ((VALUE) = 32, 1)

#define Pmode SImode

#define FUNCTION_MODE QImode

#define NO_IMPLICIT_EXTERN_C

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
