#define TARGET_OS_CPP_BUILTINS()			\
  do							\
    {							\
      NETBSD_OS_CPP_BUILTINS_AOUT();			\
      builtin_define_std ("sparc");			\
      builtin_assert ("cpu=sparc");			\
      builtin_assert ("machine=sparc");			\
    }							\
  while (0)

/* Make sure this is undefined.  */
#undef CPP_PREDEFINES

/* What extra spec entries do we need?  */
#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "netbsd_cpp_spec",          NETBSD_CPP_SPEC },

#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu) %(netbsd_cpp_spec)"

/* Make gcc agree with <machine/ansi.h> */

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

/* This is BSD, so it wants DBX format.  */

#define DBX_DEBUGGING_INFO 1

/* This is the char to use for continuation (in case we need to turn
   continuation back on).  */

#define DBX_CONTIN_CHAR '?'

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#undef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Until they use ELF or something that handles dwarf2 unwinds
   and initialization stuff better.  */
#define DWARF2_UNWIND_INFO 0

/* Attempt to enable execute permissions on the stack.  */
#define TRANSFER_FROM_TRAMPOLINE NETBSD_ENABLE_EXECUTE_STACK
