/* Assembler-independent definitions for an Intel 386 running
   Interactive Unix System V. Specifically, this is for recent versions
   that support POSIX.  */

/* Use crt1.o, not crt0.o, as a startup file, and crtn.o as a closing file. */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shlib:%{posix:%{pg:mcrtp1.o%s}%{!pg:%{p:mcrtp1.o%s}%{!p:crtp1.o%s}}}\
   %{!posix:%{pg:mcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}}\
   %{p:-L/lib/libp} %{pg:-L/lib/libp}}}\
   %{shlib:%{posix:crtp1.o%s}%{!posix:crt1.o%s}} crtbegin.o%s"
  
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

/* Library spec */
#undef LIB_SPEC
#define LIB_SPEC "%{shlib:-lc_s} %{posix:-lcposix} -lc -lg"

/* ISC 2.2 uses `char' for `wchar_t'.  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "char"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_UNIT

#if 0
/* This is apparently not true: ISC versions up to 3.0, at least, use
   the standard calling sequence in which the called function pops the
   extra arg.  */
/* caller has to pop the extra argument passed to functions that return
   structures. */

#undef RETURN_POPS_ARGS
#define RETURN_POPS_ARGS(FUNTYPE,SIZE)   \
  (TREE_CODE (FUNTYPE) == IDENTIFIER_NODE ? 0			\
   : (TARGET_RTD						\
      && (TYPE_ARG_TYPES (FUNTYPE) == 0				\
	  || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (FUNTYPE)))	\
	      == void_type_node))) ? (SIZE)			\
   : 0)
/* On other 386 systems, the last line looks like this:
   : (aggregate_value_p (TREE_TYPE (FUNTYPE))) ? GET_MODE_SIZE (Pmode) : 0)  */
#endif

/* Handle #pragma pack and #pragma weak.  */
#define HANDLE_SYSV_PRAGMA

/* By default, target has a 80387, uses IEEE compatible arithmetic,
   and returns float values in the 387, ie,
   (TARGET_80387 | TARGET_FLOAT_RETURNS_IN_80387)

   ISC's software emulation of a 387 fails to handle the `fucomp'
   opcode.  fucomp is only used when generating IEEE compliant code.
   So don't make TARGET_IEEE_FP default for ISC. */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT 0201

/* The ISC 2.0.2 software FPU emulator apparently can't handle
   80-bit XFmode insns, so don't generate them.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64
