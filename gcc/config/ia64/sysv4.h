/* Override definitions in elfos.h/svr4.h to be correct for IA64.  */

/* We want DWARF2 as specified by the IA64 ABI.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Various pseudo-ops for which the Intel assembler uses non-standard
   definitions.  */

#undef ASM_BYTE_OP
#define ASM_BYTE_OP "data1"

#undef STRING_ASM_OP
#define STRING_ASM_OP "stringz"

#undef SKIP_ASM_OP
#define SKIP_ASM_OP ".skip"

#undef COMMON_ASM_OP
#define COMMON_ASM_OP ".common"

#undef ASCII_DATA_ASM_OP
#define ASCII_DATA_ASM_OP "string"

/* ??? Unfortunately, .lcomm doesn't work, because it puts things in either
   .bss or .sbss, and we can't control the decision of which is used.  When
   I use .lcomm, I get a cryptic "Section group has no member" error from
   the Intel simulator.  So we must explicitly put variables in .bss
   instead.  This matters only if we care about the Intel assembler.  */

/* This is asm_output_aligned_bss from varasm.c without the ASM_GLOBALIZE_LABEL
   call at the beginning.  */

/* This is for final.c, because it is used by ASM_DECLARE_OBJECT_NAME.  */
extern int size_directive_output;

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN) \
do {									\
  if (XSTR (XEXP (DECL_RTL (DECL), 0), 0)[0] == SDATA_NAME_FLAG_CHAR)	\
    sbss_section ();							\
  else									\
    bss_section ();							\
  ASM_OUTPUT_ALIGN (FILE, floor_log2 ((ALIGN) / BITS_PER_UNIT));	\
  ASM_DECLARE_OBJECT_NAME (FILE, NAME, DECL);				\
  ASM_OUTPUT_SKIP (FILE, SIZE ? SIZE : 1);				\
} while (0)

/* ??? Intel assembler does not allow "." in section names, so turn off
   gnu.linkonce section support, but only when using the Intel assembler.  */
#undef UNIQUE_SECTION_P
#define UNIQUE_SECTION_P(DECL) (TARGET_GNU_AS ? DECL_ONE_ONLY (DECL) : 0)

/* The # tells the Intel assembler that this is not a register name.
   However, we can't emit the # in a label definition, so we set a variable
   in ASM_OUTPUT_LABEL to control whether we want the postfix here or not.  */

#undef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(STREAM, NAME) \
do									\
  {									\
    const char *real_name;						\
    STRIP_NAME_ENCODING (real_name, NAME);				\
    asm_fprintf (STREAM, "%U%s%s", real_name,				\
		 (ia64_asm_output_label ? "" : "#"));			\
  }									\
while (0)

/* Intel assembler requires both flags and type if declaring a non-predefined
   section.  */
#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP	".section\t.init,\"ax\",\"progbits\""
#undef FINI_SECTION_ASM_OP
#define FINI_SECTION_ASM_OP	".section\t.fini,\"ax\",\"progbits\""
#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	".section\t.ctors,\"aw\",\"progbits\""
#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	".section\t.dtors,\"aw\",\"progbits\""

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
/* Must override this to get @fptr relocation.  */
#undef ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fputs ("\tdata8\t @fptr(", FILE);					\
    assemble_name (FILE, NAME);						\
    fputs (")\n", FILE);						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
/* Must override this to get @fptr relocation.  */
#undef ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fputs ("\tdata8\t @fptr(", FILE);					\
    assemble_name (FILE, NAME);              				\
    fputs (")\n", FILE);						\
  } while (0)

/* svr4.h undefines this, so we need to define it here.  */
#define DBX_REGISTER_NUMBER(REGNO) 					\
  (IN_REGNO_P (REGNO) ? (32 + (REGNO) - IN_REG (0)) 			\
   : LOC_REGNO_P (REGNO) ? (32 + ia64_input_regs +			\
			    (REGNO) - LOC_REG (0))			\
   : OUT_REGNO_P (REGNO) ? (32 + ia64_input_regs + ia64_local_regs	\
			    + (REGNO) - OUT_REG (0))			\
   : (REGNO) == FRAME_POINTER_REGNUM ? ia64_fp_regno 			\
   : (REGNO))

/* Things that svr4.h defines to the wrong type, because it assumes 32 bit
   ints and 32 bit longs.  */

#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* We don't want any symbol at the beginning of the file.  This is defined in
   dbxelf.h which is included from elfos.h, so we need to undef/define it
   here.  */

#undef ASM_IDENTIFY_GCC
#define ASM_IDENTIFY_GCC(FILE)

/* We redefine this to use the ia64 .proc pseudo-op.  */

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL) \
do {									\
  fputs ("\t.proc ", FILE);						\
  assemble_name (FILE, NAME);						\
  fputc ('\n', FILE);							\
  ASM_OUTPUT_LABEL (FILE, NAME);					\
} while (0)

/* We redefine this to use the ia64 .endp pseudo-op.  */

#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, NAME, DECL) \
do {									\
  fputs ("\t.endp ", FILE);						\
  assemble_name (FILE, NAME);						\
  fputc ('\n', FILE);							\
} while (0)

/* A C expression which outputs to the stdio stream STREAM some appropriate
   text to go at the start of an assembler file.  */

/* ??? Looks like almost every port, except for a few original ones, get this
   wrong.  Must emit #NO_APP as first line of file to turn of special assembler
   preprocessing of files.  */

/* ??? Even worse, it doesn't work, because gas does not accept the tab chars
   that dwarf2out.c emits when #NO_APP.  */

/* ??? Unrelated, but dwarf2out.c emits unnecessary newlines after strings,
   may as well fix at the same time.  */

#if 0
#undef ASM_FILE_START
#define ASM_FILE_START(STREAM) \
do {									\
  fputs (ASM_APP_OFF, STREAM);						\
  output_file_directive (STREAM, main_input_filename);			\
} while (0)
#endif

/* Case label alignment is handled by ADDR_VEC_ALIGN now.  */

#undef ASM_OUTPUT_BEFORE_CASE_LABEL
#define ASM_OUTPUT_BEFORE_CASE_LABEL(FILE,PREFIX,NUM,TABLE)

/* We override svr4.h so that we can support the sdata section.  */

#undef SELECT_SECTION
#define SELECT_SECTION(DECL,RELOC)					\
{									\
  if (TREE_CODE (DECL) == STRING_CST)					\
    {									\
      if (! flag_writable_strings)					\
	const_section ();						\
      else								\
	data_section ();						\
    }									\
  else if (TREE_CODE (DECL) == VAR_DECL)				\
    {									\
      if (XSTR (XEXP (DECL_RTL (DECL), 0), 0)[0]			\
	  == SDATA_NAME_FLAG_CHAR)					\
        sdata_section ();						\
      /* ??? We need the extra ! RELOC check, because the default is to \
	 only check RELOC if flag_pic is set, and we don't set flag_pic \
	 (yet?).  */							\
      else if (DECL_READONLY_SECTION (DECL, RELOC) && ! (RELOC))	\
	const_section ();						\
      else								\
	data_section ();						\
    }									\
  else									\
    const_section ();							\
}

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_ctors, in_dtors, in_sdata, in_sbss

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION						\
  SDATA_SECTION_FUNCTION						\
  SBSS_SECTION_FUNCTION

#define SDATA_SECTION_ASM_OP ".sdata"

#define SDATA_SECTION_FUNCTION						\
void									\
sdata_section ()							\
{									\
  if (in_section != in_sdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", SDATA_SECTION_ASM_OP);		\
      in_section = in_sdata;						\
    }									\
}

#define SBSS_SECTION_ASM_OP ".sbss"

#define SBSS_SECTION_FUNCTION						\
void									\
sbss_section ()								\
{									\
  if (in_section != in_sbss)						\
    {									\
      fprintf (asm_out_file, "%s\n", SBSS_SECTION_ASM_OP);		\
      in_section = in_sbss;						\
    }									\
}
