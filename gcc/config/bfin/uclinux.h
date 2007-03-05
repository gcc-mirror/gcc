/* The GNU C++ standard library requires that these macros be defined.  */
#undef CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: crt1%O%s} crti%O%s crtbegin%O%s crtlibid%O%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "crtend%O%s crtn%O%s"

#undef  LIB_SPEC
#define LIB_SPEC "%{pthread:-lpthread} -lc"

#ifdef __BFIN_FDPIC__
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
asm (SECTION_OP); \
asm ("P3 = [SP + 20];\n\tcall " USER_LABEL_PREFIX #FUNC ";"); \
asm (TEXT_SECTION_ASM_OP);
#endif

#define NO_IMPLICIT_EXTERN_C

#define LINUX_TARGET_OS_CPP_BUILTINS()				\
    do {							\
	builtin_define ("__gnu_linux__");			\
	builtin_define_std ("linux");				\
	builtin_define_std ("unix");				\
	builtin_assert ("system=linux");			\
	builtin_assert ("system=unix");				\
	builtin_assert ("system=posix");			\
    } while (0)

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	LINUX_TARGET_OS_CPP_BUILTINS();		\
	if (flag_pic)				\
	  {					\
	    builtin_define ("__PIC__");		\
	    builtin_define ("__pic__");		\
	  }					\
    }						\
  while (0)
