/* Configuration for a m68k running GNU with ELF as the target machine.  */

/* This does it mostly for us.  */
#include <m68k/linux.h>

#undef CPP_PREDEFINES
#define CPP_PREDEFINES GNU_CPP_PREDEFINES("m68k")

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (m68k GNU)");

#undef	LINK_SPEC
#define LINK_SPEC "-m m68kelf %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      %{!dynamic-linker:-dynamic-linker /lib/ld.so}} \
    %{static:-static}}"


/* Get machine-independent configuration parameters for the GNU system.  */
#include <gnu.h>
