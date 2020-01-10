/* Copyright (C) 2009-2020 Free Software Foundation, Inc.
   Contributed by Anatoly Sokolov (aesok@post.ru)

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

/* Not included in avr.c since this requires C front end.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "c-family/c-common.h"
#include "stor-layout.h"
#include "langhooks.h"
#include "memmodel.h"
#include "tm_p.h"

/* IDs for all the AVR builtins.  */

enum avr_builtin_id
  {
#define DEF_BUILTIN(NAME, N_ARGS, TYPE, CODE, LIBNAME)  \
    AVR_BUILTIN_ ## NAME,
#include "builtins.def"
#undef DEF_BUILTIN

    AVR_BUILTIN_COUNT
  };


/* Implement `TARGET_RESOLVE_OVERLOADED_PLUGIN'.  */

static tree
avr_resolve_overloaded_builtin (unsigned int iloc, tree fndecl, void *vargs)
{
  tree type0, type1, fold = NULL_TREE;
  enum avr_builtin_id id = AVR_BUILTIN_COUNT;
  location_t loc = (location_t) iloc;
  vec<tree, va_gc> &args = * (vec<tree, va_gc>*) vargs;

  switch (DECL_MD_FUNCTION_CODE (fndecl))
    {
    default:
      break;

    case AVR_BUILTIN_ABSFX:
      if (args.length() != 1)
        {
          error_at (loc, "%qs expects 1 argument but %d given",
                    "absfx", (int) args.length());

          fold = error_mark_node;
          break;
        }

      type0 = TREE_TYPE (args[0]);

      if (!FIXED_POINT_TYPE_P (type0))
        {
          error_at (loc, "%qs expects a fixed-point value as argument",
                    "absfx");

          fold = error_mark_node;
        }

      switch (TYPE_MODE (type0))
        {
        case E_QQmode: id = AVR_BUILTIN_ABSHR; break;
        case E_HQmode: id = AVR_BUILTIN_ABSR; break;
        case E_SQmode: id = AVR_BUILTIN_ABSLR; break;
        case E_DQmode: id = AVR_BUILTIN_ABSLLR; break;

        case E_HAmode: id = AVR_BUILTIN_ABSHK; break;
        case E_SAmode: id = AVR_BUILTIN_ABSK; break;
        case E_DAmode: id = AVR_BUILTIN_ABSLK; break;
        case E_TAmode: id = AVR_BUILTIN_ABSLLK; break;

        case E_UQQmode:
        case E_UHQmode:
        case E_USQmode:
        case E_UDQmode:
        case E_UHAmode:
        case E_USAmode:
        case E_UDAmode:
        case E_UTAmode:
          warning_at (loc, 0, "using %qs with unsigned type has no effect",
                      "absfx");
          return args[0];

        default:
          error_at (loc, "no matching fixed-point overload found for %qs",
                    "absfx");

          fold = error_mark_node;
          break;
        }

      fold = targetm.builtin_decl (id, true);

      if (fold != error_mark_node)
        fold = build_function_call_vec (loc, vNULL, fold, &args, NULL);

      break; // absfx

    case AVR_BUILTIN_ROUNDFX:
      if (args.length() != 2)
        {
          error_at (loc, "%qs expects 2 arguments but %d given",
                    "roundfx", (int) args.length());

          fold = error_mark_node;
          break;
        }

      type0 = TREE_TYPE (args[0]);
      type1 = TREE_TYPE (args[1]);

      if (!FIXED_POINT_TYPE_P (type0))
        {
          error_at (loc, "%qs expects a fixed-point value as first argument",
                    "roundfx");

          fold = error_mark_node;
        }

      if (!INTEGRAL_TYPE_P (type1))
        {
          error_at (loc, "%qs expects an integer value as second argument",
                    "roundfx");

          fold = error_mark_node;
        }

      switch (TYPE_MODE (type0))
        {
        case E_QQmode: id = AVR_BUILTIN_ROUNDHR; break;
        case E_HQmode: id = AVR_BUILTIN_ROUNDR; break;
        case E_SQmode: id = AVR_BUILTIN_ROUNDLR; break;
        case E_DQmode: id = AVR_BUILTIN_ROUNDLLR; break;

        case E_UQQmode: id = AVR_BUILTIN_ROUNDUHR; break;
        case E_UHQmode: id = AVR_BUILTIN_ROUNDUR; break;
        case E_USQmode: id = AVR_BUILTIN_ROUNDULR; break;
        case E_UDQmode: id = AVR_BUILTIN_ROUNDULLR; break;

        case E_HAmode: id = AVR_BUILTIN_ROUNDHK; break;
        case E_SAmode: id = AVR_BUILTIN_ROUNDK; break;
        case E_DAmode: id = AVR_BUILTIN_ROUNDLK; break;
        case E_TAmode: id = AVR_BUILTIN_ROUNDLLK; break;

        case E_UHAmode: id = AVR_BUILTIN_ROUNDUHK; break;
        case E_USAmode: id = AVR_BUILTIN_ROUNDUK; break;
        case E_UDAmode: id = AVR_BUILTIN_ROUNDULK; break;
        case E_UTAmode: id = AVR_BUILTIN_ROUNDULLK; break;

        default:
          error_at (loc, "no matching fixed-point overload found for %qs",
                    "roundfx");

          fold = error_mark_node;
          break;
        }

      fold = targetm.builtin_decl (id, true);

      if (fold != error_mark_node)
        fold = build_function_call_vec (loc, vNULL, fold, &args, NULL);

      break; // roundfx

    case AVR_BUILTIN_COUNTLSFX:
      if (args.length() != 1)
        {
          error_at (loc, "%qs expects 1 argument but %d given",
                    "countlsfx", (int) args.length());

          fold = error_mark_node;
          break;
        }

      type0 = TREE_TYPE (args[0]);

      if (!FIXED_POINT_TYPE_P (type0))
        {
          error_at (loc, "%qs expects a fixed-point value as first argument",
                    "countlsfx");

          fold = error_mark_node;
        }

      switch (TYPE_MODE (type0))
        {
        case E_QQmode: id = AVR_BUILTIN_COUNTLSHR; break;
        case E_HQmode: id = AVR_BUILTIN_COUNTLSR; break;
        case E_SQmode: id = AVR_BUILTIN_COUNTLSLR; break;
        case E_DQmode: id = AVR_BUILTIN_COUNTLSLLR; break;

        case E_UQQmode: id = AVR_BUILTIN_COUNTLSUHR; break;
        case E_UHQmode: id = AVR_BUILTIN_COUNTLSUR; break;
        case E_USQmode: id = AVR_BUILTIN_COUNTLSULR; break;
        case E_UDQmode: id = AVR_BUILTIN_COUNTLSULLR; break;

        case E_HAmode: id = AVR_BUILTIN_COUNTLSHK; break;
        case E_SAmode: id = AVR_BUILTIN_COUNTLSK; break;
        case E_DAmode: id = AVR_BUILTIN_COUNTLSLK; break;
        case E_TAmode: id = AVR_BUILTIN_COUNTLSLLK; break;

        case E_UHAmode: id = AVR_BUILTIN_COUNTLSUHK; break;
        case E_USAmode: id = AVR_BUILTIN_COUNTLSUK; break;
        case E_UDAmode: id = AVR_BUILTIN_COUNTLSULK; break;
        case E_UTAmode: id = AVR_BUILTIN_COUNTLSULLK; break;

        default:
          error_at (loc, "no matching fixed-point overload found for %qs",
                    "countlsfx");

          fold = error_mark_node;
          break;
        }

      fold = targetm.builtin_decl (id, true);

      if (fold != error_mark_node)
        fold = build_function_call_vec (loc, vNULL, fold, &args, NULL);

      break; // countlsfx
    }

  return fold;
}
  

/* Implement `REGISTER_TARGET_PRAGMAS'.  */

void
avr_register_target_pragmas (void)
{
  gcc_assert (ADDR_SPACE_GENERIC == ADDR_SPACE_RAM);

  /* Register address spaces.  The order must be the same as in the respective
     enum from avr.h (or designated initializers must be used in avr.c).
     We always register all address spaces even if some of them make no
     sense for some targets.  Diagnose for non-supported spaces will be
     emit by TARGET_ADDR_SPACE_DIAGNOSE_USAGE.  */

  for (int i = 0; i < ADDR_SPACE_COUNT; i++)
    {
      gcc_assert (i == avr_addrspace[i].id);

      if (!ADDR_SPACE_GENERIC_P (i))
        c_register_addr_space (avr_addrspace[i].name, avr_addrspace[i].id);
    }

  targetm.resolve_overloaded_builtin = avr_resolve_overloaded_builtin;
}


/* Transform LO into uppercase and write the result to UP.
   You must provide enough space for UP.  Return UP.  */

static char*
avr_toupper (char *up, const char *lo)
{
  char *up0 = up;

  for (; *lo; lo++, up++)
    *up = TOUPPER (*lo);

  *up = '\0';

  return up0;
}

/* Worker function for TARGET_CPU_CPP_BUILTINS.  */

void
avr_cpu_cpp_builtins (struct cpp_reader *pfile)
{
  builtin_define_std ("AVR");

  /* __AVR_DEVICE_NAME__ and  avr_mcu_types[].macro like __AVR_ATmega8__
     are defined by -D command option, see device-specs file.  */

  if (avr_arch->macro)
    cpp_define_formatted (pfile, "__AVR_ARCH__=%s", avr_arch->macro);
  if (AVR_HAVE_RAMPD)    cpp_define (pfile, "__AVR_HAVE_RAMPD__");
  if (AVR_HAVE_RAMPX)    cpp_define (pfile, "__AVR_HAVE_RAMPX__");
  if (AVR_HAVE_RAMPY)    cpp_define (pfile, "__AVR_HAVE_RAMPY__");
  if (AVR_HAVE_RAMPZ)    cpp_define (pfile, "__AVR_HAVE_RAMPZ__");
  if (AVR_HAVE_ELPM)     cpp_define (pfile, "__AVR_HAVE_ELPM__");
  if (AVR_HAVE_ELPMX)    cpp_define (pfile, "__AVR_HAVE_ELPMX__");
  if (AVR_HAVE_MOVW)     cpp_define (pfile, "__AVR_HAVE_MOVW__");
  if (AVR_HAVE_LPMX)     cpp_define (pfile, "__AVR_HAVE_LPMX__");

  if (avr_arch->asm_only)
    cpp_define (pfile, "__AVR_ASM_ONLY__");
  if (AVR_HAVE_MUL)
    {
      cpp_define (pfile, "__AVR_ENHANCED__");
      cpp_define (pfile, "__AVR_HAVE_MUL__");
    }

  if (AVR_HAVE_JMP_CALL)
    cpp_define (pfile, "__AVR_HAVE_JMP_CALL__");

  if (avr_arch->have_jmp_call)
    cpp_define (pfile, "__AVR_MEGA__");

  if (AVR_SHORT_CALLS)
    cpp_define (pfile, "__AVR_SHORT_CALLS__");

  if (AVR_XMEGA)
    cpp_define (pfile, "__AVR_XMEGA__");

  if (AVR_TINY)
    {
      cpp_define (pfile, "__AVR_TINY__");

      /* Define macro "__AVR_TINY_PM_BASE_ADDRESS__" with mapped program memory
         start address.  This macro shall be used where mapped program
         memory is accessed, eg. copying data section (__do_copy_data)
         contents to data memory region.
         NOTE:
         Program memory of AVR_TINY devices cannot be accessed directly,
         it has been mapped to the data memory.  For AVR_TINY devices
         (ATtiny4/5/9/10/20 and 40) mapped program memory starts at 0x4000. */

      cpp_define_formatted (pfile, "__AVR_TINY_PM_BASE_ADDRESS__=0x%x",
                            avr_arch->flash_pm_offset);
    }

  if (avr_arch->flash_pm_offset)
    cpp_define_formatted (pfile, "__AVR_PM_BASE_ADDRESS__=0x%x",
                          avr_arch->flash_pm_offset);

  if (AVR_HAVE_EIJMP_EICALL)
    {
      cpp_define (pfile, "__AVR_HAVE_EIJMP_EICALL__");
      cpp_define (pfile, "__AVR_3_BYTE_PC__");
    }
  else
    {
      cpp_define (pfile, "__AVR_2_BYTE_PC__");
    }

  if (AVR_HAVE_8BIT_SP)
    cpp_define (pfile, "__AVR_HAVE_8BIT_SP__");
  else
    cpp_define (pfile, "__AVR_HAVE_16BIT_SP__");

  if (AVR_HAVE_SPH)
    cpp_define (pfile, "__AVR_HAVE_SPH__");
  else
    cpp_define (pfile, "__AVR_SP8__");

  if (TARGET_NO_INTERRUPTS)
    cpp_define (pfile, "__NO_INTERRUPTS__");

  if (TARGET_SKIP_BUG)
    {
      cpp_define (pfile, "__AVR_ERRATA_SKIP__");

      if (AVR_HAVE_JMP_CALL)
        cpp_define (pfile, "__AVR_ERRATA_SKIP_JMP_CALL__");
    }

  if (TARGET_RMW)
    cpp_define (pfile, "__AVR_ISA_RMW__");

  cpp_define_formatted (pfile, "__AVR_SFR_OFFSET__=0x%x",
                        avr_arch->sfr_offset);

#ifdef WITH_AVRLIBC
  cpp_define (pfile, "__WITH_AVRLIBC__");
#endif /* WITH_AVRLIBC */

  // From configure --with-libf7={|libgcc|math|math-symbols|yes|no}

#ifdef WITH_LIBF7_LIBGCC
  cpp_define (pfile, "__WITH_LIBF7_LIBGCC__");
#endif /* WITH_LIBF7_LIBGCC */

#ifdef WITH_LIBF7_MATH
  cpp_define (pfile, "__WITH_LIBF7_MATH__");
#endif /* WITH_LIBF7_MATH */

#ifdef WITH_LIBF7_MATH_SYMBOLS
  cpp_define (pfile, "__WITH_LIBF7_MATH_SYMBOLS__");
#endif /* WITH_LIBF7_MATH_SYMBOLS */

  // From configure --with-double={|32|32,64|64,32|64}

#ifdef HAVE_DOUBLE_MULTILIB
  cpp_define (pfile, "__HAVE_DOUBLE_MULTILIB__");
#endif

#ifdef HAVE_DOUBLE64
  cpp_define (pfile, "__HAVE_DOUBLE64__");
#endif

#ifdef HAVE_DOUBLE32
  cpp_define (pfile, "__HAVE_DOUBLE32__");
#endif

#if defined (WITH_DOUBLE64)
  cpp_define (pfile, "__DEFAULT_DOUBLE__=64");
#elif defined (WITH_DOUBLE32)
  cpp_define (pfile, "__DEFAULT_DOUBLE__=32");
#else
#error "align this with config.gcc"
#endif

  // From configure --with-long-double={|32|32,64|64,32|64|double}

#ifdef HAVE_LONG_DOUBLE_MULTILIB
  cpp_define (pfile, "__HAVE_LONG_DOUBLE_MULTILIB__");
#endif

#ifdef HAVE_LONG_DOUBLE64
  cpp_define (pfile, "__HAVE_LONG_DOUBLE64__");
#endif

#ifdef HAVE_LONG_DOUBLE32
  cpp_define (pfile, "__HAVE_LONG_DOUBLE32__");
#endif

#ifdef HAVE_LONG_DOUBLE_IS_DOUBLE
  cpp_define (pfile, "__HAVE_LONG_DOUBLE_IS_DOUBLE__");
#endif

#if defined (WITH_LONG_DOUBLE64)
  cpp_define (pfile, "__DEFAULT_LONG_DOUBLE__=64");
#elif defined (WITH_LONG_DOUBLE32)
  cpp_define (pfile, "__DEFAULT_LONG_DOUBLE__=32");
#else
#error "align this with config.gcc"
#endif

  // From configure --with-double-comparison={2|3} --with-libf7.

#if defined (WITH_DOUBLE_COMPARISON)
#if WITH_DOUBLE_COMPARISON == 2 || WITH_DOUBLE_COMPARISON == 3
  /* The number of states a DFmode comparison libcall might take and
     reflects what avr.c:FLOAT_LIB_COMPARE_RETURNS_BOOL returns for
     DFmode.  GCC's default is 3-state, but some libraries like LibF7
     implement true / false (2-state).  */
  cpp_define_formatted (pfile, "__WITH_DOUBLE_COMPARISON__=%d",
			WITH_DOUBLE_COMPARISON);
#else
#error "align this with config.gcc"
#endif
#else
#error "align this with config.gcc"
#endif

  /* Define builtin macros so that the user can easily query whether
     non-generic address spaces (and which) are supported or not.
     This is only supported for C.  For C++, a language extension is needed
     (as mentioned in ISO/IEC DTR 18037; Annex F.2) which is not
     implemented in GCC up to now.  */

  if (lang_GNU_C ())
    {
      for (int i = 0; i < ADDR_SPACE_COUNT; i++)
        if (!ADDR_SPACE_GENERIC_P (i)
            /* Only supply __FLASH<n> macro if the address space is reasonable
               for this target.  The address space qualifier itself is still
               supported, but using it will throw an error.  */
            && avr_addr_space_supported_p ((addr_space_t) i))
          {
            const char *name = avr_addrspace[i].name;
            char *Name = (char*) alloca (1 + strlen (name));

            cpp_define (pfile, avr_toupper (Name, name));
          }
    }

  /* Define builtin macros so that the user can easily query whether or
     not a specific builtin is available. */

#define DEF_BUILTIN(NAME, N_ARGS, TYPE, CODE, LIBNAME)  \
  cpp_define (pfile, "__BUILTIN_AVR_" #NAME);
#include "builtins.def"
#undef DEF_BUILTIN

  /* Builtin macros for the __int24 and __uint24 type.  */

  cpp_define_formatted (pfile, "__INT24_MAX__=8388607%s",
                        INT_TYPE_SIZE == 8 ? "LL" : "L");
  cpp_define (pfile, "__INT24_MIN__=(-__INT24_MAX__-1)");
  cpp_define_formatted (pfile, "__UINT24_MAX__=16777215%s",
                        INT_TYPE_SIZE == 8 ? "ULL" : "UL");
}
