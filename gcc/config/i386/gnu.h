/* Configuration for an i386 running GNU with ELF as the target machine.  */

/* This does it mostly for us.  */
#include <i386/linux.h>

#undef CPP_PREDEFINES
#define CPP_PREDEFINES GNU_CPP_PREDEFINES("i386")

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i386 GNU)");

#undef	LINK_SPEC
#define LINK_SPEC "-m elf_i386 %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      %{!dynamic-linker:-dynamic-linker /lib/ld.so}} \
    %{static:-static}}"


/* Get machine-independent configuration parameters for the GNU system.  */
#include <gnu.h>
