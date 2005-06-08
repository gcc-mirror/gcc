/* Definitions of target machine for GCC, for SPARC running Solaris 2
   using the GNU assembler.  */

/* Undefine this so that BNSYM/ENSYM pairs are emitted by STABS+.  */
#undef NO_DBX_BNSYM_ENSYM

/* Emit a DTP-relative reference to a TLS variable.  */
#ifdef HAVE_AS_TLS
#define ASM_OUTPUT_DWARF_DTPREL(FILE, SIZE, X) \
  sparc_output_dwarf_dtprel (FILE, SIZE, X)
#endif
