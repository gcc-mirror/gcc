#undef  STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: crt1%O%s} crti%O%s crtbegin%O%s crtlibid%O%s"

#define TARGET_OS_CPP_BUILTINS() LINUX_TARGET_OS_CPP_BUILTINS()

#define MD_UNWIND_SUPPORT "config/bfin/linux-unwind.h"

/* Like the definition in gcc.c, but for purposes of uClinux, every link is
   static.  */
#define MFWRAP_SPEC " %{fmudflap|fmudflapth: \
 --wrap=malloc --wrap=free --wrap=calloc --wrap=realloc\
 --wrap=mmap --wrap=munmap --wrap=alloca\
 %{fmudflapth: --wrap=pthread_create\
}} %{fmudflap|fmudflapth: --wrap=main}"
