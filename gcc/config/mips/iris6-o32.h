/* Definitions of target machine for GNU compiler, for MIPS running IRIX 6
   (O32 ABI).  */

/* The O32 ABI on IRIX 6 defaults to the mips2 ISA.  */
#undef MIPS_CPU_STRING_DEFAULT
#define MIPS_CPU_STRING_DEFAULT "mips2"

/* Specify wchar_t and wint_t types.  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef WINT_TYPE
#define WINT_TYPE "long int"

#undef WINT_TYPE_SIZE
#define WINT_TYPE_SIZE 32

/* Enforce use of O32 assembler, irrespective of SGI_ABI environment variable
   and machine type (e.g., R8000 systems default to -64).  Gas doesn't need
   this, but doesn't hurt either.  Need to pass -mips2 to gas which defaults
   to -mips1 if no ISA is specified.  */
#undef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC "-32 %{!mips*:-mips2}"
