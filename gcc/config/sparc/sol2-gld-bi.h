/* Definitions of target machine for GCC, for bi-arch SPARC
   running Solaris 2 using the GNU linker.  */

#undef LINK_ARCH32_SPEC
#define LINK_ARCH32_SPEC \
  LINK_ARCH32_SPEC_BASE "%{!static: -rpath-link %R/usr/lib}"

#undef LINK_ARCH64_SPEC
#define LINK_ARCH64_SPEC \
  LINK_ARCH64_SPEC_BASE "%{!static: -rpath-link %R/usr/lib/sparcv9}"

/* Since binutils 2.21, GNU ld supports new *_sol2 emulations to strictly
   follow the Solaris 2 ABI.  Prefer them if present.  */
#ifdef HAVE_LD_SOL2_EMULATION
#define SPARC32_EMULATION "elf32_sparc_sol2"
#define SPARC64_EMULATION "elf64_sparc_sol2"
#else
#define SPARC32_EMULATION "elf32_sparc"
#define SPARC64_EMULATION "elf64_sparc"
#endif

#undef LINK_ARCH_SPEC
#if DISABLE_MULTILIB
#if DEFAULT_ARCH32_P
#define LINK_ARCH_SPEC "\
%{m32:-m " SPARC32_EMULATION " %(link_arch32)} \
%{m64:%edoes not support multilib} \
%{!m32:%{!m64:%(link_arch_default)}} \
"
#else
#define LINK_ARCH_SPEC "\
%{m32:%edoes not support multilib} \
%{m64:-m " SPARC64_EMULATION " %(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}} \
"
#endif
#else
#define LINK_ARCH_SPEC "\
%{m32:-m " SPARC32_EMULATION " %(link_arch32)} \
%{m64:-m " SPARC64_EMULATION " %(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}} \
"
#endif

