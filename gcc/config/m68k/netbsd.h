#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      NETBSD_OS_CPP_BUILTINS_AOUT();		\
      builtin_define_std ("unix");		\
      builtin_define_std ("m68k");		\
      builtin_define_std ("mc68000");		\
      builtin_define_std ("mc68020");		\
    }						\
  while (0)

#define TARGET_DEFAULT (MASK_BITFIELD|MASK_68881|MASK_68020)

#define EXTRA_SPECS \
  { "netbsd_cpp_spec",      NETBSD_CPP_SPEC },

/* Define __HAVE_68881__ in preprocessor, unless -msoft-float is specified.
   This will control the use of inline 68881 insns in certain macros.  */

#undef CPP_SPEC
#define CPP_SPEC \
  "%{!msoft-float:-D__HAVE_68881__ -D__HAVE_FPU__} %(netbsd_cpp_spec)"

#undef ASM_SPEC
#define ASM_SPEC "%{m68030} %{m68040} %{m68060} %{fpic|fpie:-k} %{fPIC|fPIE:-k -K}"

#define AS_NEEDS_DASH_FOR_PIPED_INPUT


/* Make gcc agree with <machine/ansi.h> */

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* This is BSD, so it wants DBX format.  */

#define DBX_DEBUGGING_INFO 1

/* Do not break .stabs pseudos into continuations.  */

#define DBX_CONTIN_LENGTH 0

/* This is the char to use for continuation (in case we need to turn
   continuation back on).  */

#define DBX_CONTIN_CHAR '?'

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Until they use ELF or something that handles dwarf2 unwinds
   and initialization stuff better.  */
#define DWARF2_UNWIND_INFO 0

