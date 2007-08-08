/* Configuration for an i386 running GNU with ELF as the target machine.  */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i386 GNU)");

#undef TARGET_OS_CPP_BUILTINS /* config.gcc includes i386/linux.h.  */
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	HURD_TARGET_OS_CPP_BUILTINS();		\
    }						\
  while (0)

#undef CPP_SPEC
#define CPP_SPEC "%{pthread:-D_REENTRANT} %{posix:-D_POSIX_SOURCE} %{bsd:-D_BSD_SOURCE}"

#undef CC1_SPEC
#define CC1_SPEC "%(cc1_cpu)"

#undef	LINK_SPEC
#define LINK_SPEC "-m elf_i386 %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      %{!dynamic-linker:-dynamic-linker /lib/ld.so}} \
    %{static:-static}}"

#undef	STARTFILE_SPEC
#if defined HAVE_LD_PIE
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt0.o%s;pie:Scrt1.o%s;static:crt0.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#else
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt0.o%s;static:crt0.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#endif

#undef	ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"

/* FIXME: Is a Hurd-specific fallback mechanism necessary?  */
#undef MD_UNWIND_SUPPORT
