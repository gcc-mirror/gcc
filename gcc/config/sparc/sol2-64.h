/* Definitions of target machine for GNU compiler, for 64-bit SPARC
   running Solaris 2 using the GNU linker.  */

#ifdef LINKER_DOES_NOT_WORK_WITH_DWARF2
#undef LINKER_DOES_NOT_WORK_WITH_DWARF2
#endif

#ifdef AS_SPARC64_FLAG

#ifdef LINK_ARCH_SPEC
#undef LINK_ARCH_SPEC
#endif

#define LINK_ARCH_SPEC "\
%{m32:-m elf32_sparc %(link_arch32)} \
%{m64:-m elf64_sparc %(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}} \
"

#endif
