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

/* Copied from iris5.h, with _MIPS_SIM definition adapted to SGI cc usage
   and -D_LONGLONG added as in iris6.h.  */
#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()			\
    do {						\
	builtin_define_std ("host_mips");		\
	builtin_define_std ("sgi");			\
	builtin_define_std ("unix");			\
	builtin_define_std ("SYSTYPE_SVR4");		\
	builtin_define ("_LONGLONG");			\
	builtin_define ("_MODERN_C");			\
	builtin_define ("_SVR4_SOURCE");		\
	builtin_define ("__DSO__");			\
	builtin_define ("_ABIO32=1");			\
	builtin_define ("_MIPS_SIM=_ABIO32");		\
	builtin_define ("_MIPS_SZPTR=32");		\
	builtin_assert ("system=unix");			\
	builtin_assert ("system=svr4");			\
	builtin_assert ("machine=sgi");			\
							\
     if (!TARGET_FLOAT64)                               \
        builtin_define ("_MIPS_FPSET=16");              \
     else                                               \
        builtin_define ("_MIPS_FPSET=32");              \
							\
     if (!TARGET_INT64)                                 \
        builtin_define ("_MIPS_SZINT=32");              \
     else                                               \
        builtin_define ("_MIPS_SZINT=64");              \
							\
     if (!TARGET_LONG64)				\
	builtin_define ("_MIPS_SZLONG=32");		\
     else						\
	builtin_define ("_MIPS_SZLONG=64");		\
							\
     if (!flag_iso)					\
       {						\
	 builtin_define ("__EXTENSIONS__");		\
	 builtin_define ("_SGI_SOURCE");		\
       }						\
} while (0);

/* Enforce use of O32 assembler, irrespective of SGI_ABI environment variable
   and machine type (e.g., R8000 systems default to -64).  Gas doesn't need
   this, but doesn't hurt either.  Need to pass -mips2 to gas which defaults
   to -mips1 if no ISA is specified.  */
#undef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC "-32 %{!mips*:-mips2}"
