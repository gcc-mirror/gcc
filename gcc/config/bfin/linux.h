#undef SUBTARGET_DRIVER_SELF_SPECS
#define SUBTARGET_DRIVER_SELF_SPECS \
  "%{!mno-fdpic:-mfdpic}",

/* The GNU C++ standard library requires that these macros be defined.  */
#undef CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

#ifdef __BFIN_FDPIC__
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
asm (SECTION_OP); \
asm ("P3 = [SP + 20];\n\tcall " USER_LABEL_PREFIX #FUNC ";"); \
asm (TEXT_SECTION_ASM_OP);
#endif

#define NO_IMPLICIT_EXTERN_C

#undef TARGET_OS_CPP_BUILTINS
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

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;pie:Scrt1.o%s;:crt1.o%s}} crtreloc.o%s \
   crti.o%s %{shared|pie:crtbeginS.o%s;:crtbegin.o%s}"

#undef LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC \
  "%{mfast-fp:-lbffastfp} %{static:--start-group} %G %L %{static:--end-group}%{!static:%G}"

#undef LINK_SPEC
#define LINK_SPEC "\
  %{mfdpic: -m elf32bfinfd -z text} %{shared} %{pie} \
  %{static:-dn -Bstatic} \
  %{shared:-G -Bdynamic} \
  %{!shared: %{!static: \
   %{rdynamic:-export-dynamic} \
   %{!dynamic-linker:-dynamic-linker /lib/ld-uClibc.so.0}} \
   %{static}} -init __init -fini __fini"

