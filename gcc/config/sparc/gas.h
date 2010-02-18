/* Definitions of target machine for GCC, for SPARC
   using the GNU assembler.  */

/* Switch into a generic section.  */
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION  default_elf_asm_named_section
