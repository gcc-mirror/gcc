/* Configuration for an i386 running GNU with ELF as the target machine.  */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i386 GNU)");

#undef TARGET_OS_CPP_BUILTINS /* config.gcc includes i386/linux.h.  */
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define_std ("MACH");		\
	builtin_define_std ("unix");		\
	builtin_define ("__ELF__");		\
	builtin_define ("__GNU__");		\
	builtin_define ("__gnu_hurd__");	\
	builtin_assert ("system=gnu");		\
	builtin_assert ("system=mach");		\
	builtin_assert ("system=posix");	\
	builtin_assert ("system=unix");		\
	if (flag_pic)				\
	  {					\
	    builtin_define ("__PIC__");		\
	    builtin_define ("__pic__");		\
	  }					\
    }						\
  while (0)

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{bsd:-D_BSD_SOURCE}"

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
#define STARTFILE_SPEC \
  "%{!shared: \
     %{!static: \
       %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} %{!p:crt1.o%s}}} \
     %{static:crt0.o%s}} \
   crti.o%s %{static:crtbeginT.o%s}\
   %{!static:%{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}}"

/* FIXME: Is a Hurd-specific fallback mechanism necessary?  */
#undef MD_FALLBACK_FRAME_STATE_FOR
