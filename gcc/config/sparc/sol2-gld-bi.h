/* Definitions of target machine for GNU compiler, for bi-arch SPARC
   running Solaris 2 using the GNU linker.  */

#undef LINK_ARCH_SPEC
#define LINK_ARCH_SPEC "\
%{m32:-m elf32_sparc %(link_arch32)} \
%{m64:-m elf64_sparc %(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}} \
"
