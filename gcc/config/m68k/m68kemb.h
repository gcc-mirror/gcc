/* Definitions of target machine for GNU compiler.  "embedded" 68XXX.
   This is meant to be included after m68k.h.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.  */

#define PTRDIFF_TYPE "long int"
#define SIZE_TYPE "long unsigned int"

/* In order for bitfields to work on a 68000, or with -mnobitfield, we must
   define either PCC_BITFIELD_TYPE_MATTERS or STRUCTURE_SIZE_BOUNDARY.
   Defining STRUCTURE_SIZE_BOUNDARY results in structure packing problems,
   so we define PCC_BITFIELD_TYPE_MATTERS.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Undef PCC_STATIC_STRUCT_RETURN so that we get a re-entrant calling
   convention.  */
#undef PCC_STATIC_STRUCT_RETURN

/* Don't default to pcc-struct-return, so that we can return small structures
   and unions in registers, which is slightly more efficient.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Return floating point values in a fp register.  This make fp code a
   little bit faster.  It also makes -msoft-float code incompatible with
   -m68881 code, so people have to be careful not to mix the two.  */
#undef FUNCTION_VALUE
#define FUNCTION_VALUE(VALTYPE,FUNC) LIBCALL_VALUE (TYPE_MODE (VALTYPE))

#undef LIBCALL_VALUE
#define LIBCALL_VALUE(MODE)                                                \
 gen_rtx (REG, (MODE),                                                     \
          ((TARGET_68881                                                   \
            && ((MODE) == SFmode || (MODE) == DFmode || (MODE) == XFmode)) \
           ? 16 : 0))

#undef FUNCTION_VALUE_REGNO_P
#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0 || (TARGET_68881 && (N) == 16))

#undef NEEDS_UNTYPED_CALL
#define NEEDS_UNTYPED_CALL 1

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmc68000 -D__embedded__ -Asystem(embedded) \
  -Amachine(mc68000)"

/* Override the default LIB_SPEC from gcc.c.  We don't currently support
   profiling, or libg.a.  */

#undef  LIB_SPEC
#define LIB_SPEC "-lc"

/* Make this be null, since we want the crt0.o to come from the linker
   script */

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC ""
