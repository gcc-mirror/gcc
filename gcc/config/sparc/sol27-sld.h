/* Up through Solaris 2.7, the system linker does not work with DWARF
   or DWARF2, since it does not have working support for relocations
   to unaligned data.  */

#undef DWARF_DEBUGGING_INFO
#undef DWARF2_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE
#undef ASM_DEBUG_SPEC
