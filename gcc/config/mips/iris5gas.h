/* Definitions of target machine for GNU compiler.  Irix version 5 with gas.  */

/* Enable debugging.  */
#define DBX_DEBUGGING_INFO 1
#define DWARF2_DEBUGGING_INFO 1
#define SDB_DEBUGGING_INFO 1
#define MIPS_DEBUGGING_INFO 1
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* GNU as does handle DWARF2 directives.  */
#undef DWARF2_UNWIND_INFO
#define DWARF2_UNWIND_INFO 1

/* Override mips.h version to match DWARF 2 default.  */
#undef MDEBUG_ASM_SPEC
#define MDEBUG_ASM_SPEC "%{gstabs*|gcoff*:-mdebug} \
%{!gstabs*:%{!gcoff*:-no-mdebug}}"

/* Override iris5.h version to invoke [cd]tors and register eh frame
   information.  */
#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} \
%{static: -non_shared} \
%{!static: \
  %{!shared:%{!non_shared:%{!call_shared: -call_shared -no_unresolved}}}} \
%{rpath} -init __do_global_ctors -fini __do_global_dtors \
%{shared:-hidden_symbol __do_global_ctors,__do_global_ctors_1,__do_global_dtors} \
-_SYSTYPE_SVR4"

/* Override iris5.h versions to include crtbegin.o and crtend.o.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "\
%{!static: \
  %{!shared:%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s libprof1.a%s}%{!p:crt1.o%s}}}} \
%{static: \
  %{pg:gcrt1.o%s} \
  %{!pg:%{p:/usr/lib/nonshared/mcrt1.o%s libprof1.a%s} \
  %{!p:/usr/lib/nonshared/crt1.o%s}}} \
crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s %{!shared:crtn.o%s}"

/* Irix 5 does not have some strange restrictions that Irix 3 had.  */
#undef SET_FILE_NUMBER
#define SET_FILE_NUMBER() ++num_source_filenames
#undef LABEL_AFTER_LOC
#define LABEL_AFTER_LOC(STREAM)

/* We need to use .esize and .etype instead of .size and .type to
   avoid conflicting with ELF directives.  These are only recognized
   by gas, anyhow, not the native assembler.  */
#undef PUT_SDB_SIZE
#define PUT_SDB_SIZE(a)                                       \
do {                                                  \
  extern FILE *asm_out_text_file;                     \
  fprintf (asm_out_text_file, "\t.esize\t");          \
  fprintf (asm_out_text_file, HOST_WIDE_INT_PRINT_DEC, (HOST_WIDE_INT) (a)); \
  fprintf (asm_out_text_file, ";");                   \
} while (0)

#undef PUT_SDB_TYPE
#define PUT_SDB_TYPE(a)                                       \
do {                                                  \
  extern FILE *asm_out_text_file;                     \
  fprintf (asm_out_text_file, "\t.etype\t0x%x;", (a));        \
} while (0)

/* Switch into a generic section.  */
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION  default_elf_asm_named_section

/* Add -g to mips.h default to avoid confusing gas with local symbols
   generated from stabs info.  */
#undef NM_FLAGS
#define NM_FLAGS "-Bng"

/* Disable SHF_MERGE support.  Even if gas supports it, the IRIX ld does not
   without a special elspec(5) file.  */
#undef HAVE_GAS_SHF_MERGE
