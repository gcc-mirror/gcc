#undef SUBTARGET_DRIVER_SELF_SPECS
#define SUBTARGET_DRIVER_SELF_SPECS \
  "%{!mno-fdpic:-mfdpic}",

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS() LINUX_TARGET_OS_CPP_BUILTINS()

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;pie:Scrt1.o%s;:crt1.o%s}} crtreloc.o%s \
   crti.o%s %{shared|pie:crtbeginS.o%s;:crtbegin.o%s}"

#undef LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC \
  "%{static:--start-group} %{mfast-fp:-lbffastfp} %G %L %{static:--end-group} \
   %{!static:%{mfast-fp:-lbffastfp} %G}"

#undef LINK_SPEC
#define LINK_SPEC "\
  %{mfdpic: -m elf32bfinfd -z text} %{shared} %{pie} \
  %{static:-dn -Bstatic} \
  %{shared:-G -Bdynamic} \
  %{!shared: %{!static: \
   %{rdynamic:-export-dynamic} \
   %{!dynamic-linker:-dynamic-linker /lib/ld-uClibc.so.0}} \
   %{static}} -init __init -fini __fini"

#define MD_UNWIND_SUPPORT "config/bfin/linux-unwind.h"
