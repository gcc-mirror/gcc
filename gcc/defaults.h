/* Definitions of various defaults for tm.h macros.
   Copyright (C) 1992, 1996, 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@monkeys.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_DEFAULTS_H
#define GCC_DEFAULTS_H

#ifndef GET_ENVIRONMENT
#define GET_ENVIRONMENT(VALUE, NAME) do { (VALUE) = getenv (NAME); } while (0)
#endif

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* Define default standard character escape sequences.  */
#ifndef TARGET_BELL
#  define TARGET_BELL 007
#  define TARGET_BS 010
#  define TARGET_TAB 011
#  define TARGET_NEWLINE 012
#  define TARGET_VT 013
#  define TARGET_FF 014
#  define TARGET_CR 015
#  define TARGET_ESC 033
#endif

/* When removal of CPP_PREDEFINES is complete, TARGET_CPU_CPP_BULITINS
   can also be removed from here.  */
#ifndef TARGET_OS_CPP_BUILTINS
# define TARGET_OS_CPP_BUILTINS()
#endif
#ifndef TARGET_CPU_CPP_BUILTINS
# define TARGET_CPU_CPP_BUILTINS()
#endif
#ifndef CPP_PREDEFINES
# define CPP_PREDEFINES ""
#endif

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable or function named NAME.
   LABELNO is an integer which is different for each call.  */

#ifndef ASM_FORMAT_PRIVATE_NAME
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)			\
  do {									\
    int len = strlen (NAME);						\
    char *temp = (char *) alloca (len + 3);				\
    temp[0] = 'L';							\
    strcpy (&temp[1], (NAME));						\
    temp[len + 1] = '.';						\
    temp[len + 2] = 0;							\
    (OUTPUT) = (char *) alloca (strlen (NAME) + 11);			\
    ASM_GENERATE_INTERNAL_LABEL (OUTPUT, temp, LABELNO);		\
  } while (0)
#endif

#ifndef ASM_STABD_OP
#define ASM_STABD_OP "\t.stabd\t"
#endif

/* This is how to output an element of a case-vector that is absolute.
   Some targets don't use this, but we have to define it anyway.  */

#ifndef ASM_OUTPUT_ADDR_VEC_ELT
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
do { fputs (integer_asm_op (POINTER_SIZE / UNITS_PER_WORD, TRUE), FILE); \
     ASM_OUTPUT_INTERNAL_LABEL (FILE, "L", (VALUE));			\
     fputc ('\n', FILE);						\
   } while (0)
#endif

/* choose a reasonable default for ASM_OUTPUT_ASCII.  */

#ifndef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(MYFILE, MYSTRING, MYLENGTH) \
  do {									      \
    FILE *_hide_asm_out_file = (MYFILE);				      \
    const unsigned char *_hide_p = (const unsigned char *) (MYSTRING);	      \
    int _hide_thissize = (MYLENGTH);					      \
    {									      \
      FILE *asm_out_file = _hide_asm_out_file;				      \
      const unsigned char *p = _hide_p;					      \
      int thissize = _hide_thissize;					      \
      int i;								      \
      fprintf (asm_out_file, "\t.ascii \"");				      \
									      \
      for (i = 0; i < thissize; i++)					      \
	{								      \
	  int c = p[i];			   				      \
	  if (c == '\"' || c == '\\')					      \
	    putc ('\\', asm_out_file);					      \
	  if (ISPRINT(c))						      \
	    putc (c, asm_out_file);					      \
	  else								      \
	    {								      \
	      fprintf (asm_out_file, "\\%o", c);			      \
	      /* After an octal-escape, if a digit follows,		      \
		 terminate one string constant and start another.	      \
		 The VAX assembler fails to stop reading the escape	      \
		 after three digits, so this is the only way we		      \
		 can get it to parse the data properly.  */		      \
	      if (i < thissize - 1 && ISDIGIT(p[i + 1]))		      \
		fprintf (asm_out_file, "\"\n\t.ascii \"");		      \
	  }								      \
	}								      \
      fprintf (asm_out_file, "\"\n");					      \
    }									      \
  }									      \
  while (0)
#endif

/* This is how we tell the assembler to equate two values.  */
#ifdef SET_ASM_OP
#ifndef ASM_OUTPUT_DEF
#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
 do {	fprintf ((FILE), "%s", SET_ASM_OP);				\
	assemble_name (FILE, LABEL1);					\
	fprintf (FILE, ",");						\
	assemble_name (FILE, LABEL2);					\
	fprintf (FILE, "\n");						\
  } while (0)
#endif
#endif

/* This is how to output the definition of a user-level label named
   NAME, such as the label on a static function or variable NAME.  */

#ifndef ASM_OUTPUT_LABEL
#define ASM_OUTPUT_LABEL(FILE,NAME) \
  do { assemble_name ((FILE), (NAME)); fputs (":\n", (FILE)); } while (0)
#endif

/* This is how to output a reference to a user-level label named NAME.  */

#ifndef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME)  asm_fprintf ((FILE), "%U%s", (NAME))
#endif

/* Allow target to print debug info labels specially.  This is useful for
   VLIW targets, since debug info labels should go into the middle of
   instruction bundles instead of breaking them.  */

#ifndef ASM_OUTPUT_DEBUG_LABEL
#define ASM_OUTPUT_DEBUG_LABEL(FILE, PREFIX, NUM) \
  ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM)
#endif

/* This is how we tell the assembler that a symbol is weak.  */
#ifndef ASM_OUTPUT_WEAK_ALIAS
#if defined (ASM_WEAKEN_LABEL) && defined (ASM_OUTPUT_DEF)
#define ASM_OUTPUT_WEAK_ALIAS(STREAM, NAME, VALUE)	\
  do							\
    {							\
      ASM_WEAKEN_LABEL (STREAM, NAME);			\
      if (VALUE)					\
        ASM_OUTPUT_DEF (STREAM, NAME, VALUE);		\
    }							\
  while (0)
#endif
#endif

/* How to emit a .type directive.  */
#ifndef ASM_OUTPUT_TYPE_DIRECTIVE
#if defined TYPE_ASM_OP && defined TYPE_OPERAND_FMT
#define ASM_OUTPUT_TYPE_DIRECTIVE(STREAM, NAME, TYPE)	\
  do							\
    {							\
      fputs (TYPE_ASM_OP, STREAM);			\
      assemble_name (STREAM, NAME);			\
      fputs (", ", STREAM);				\
      fprintf (STREAM, TYPE_OPERAND_FMT, TYPE);		\
      putc ('\n', STREAM);				\
    }							\
  while (0)
#endif
#endif

/* How to emit a .size directive.  */
#ifndef ASM_OUTPUT_SIZE_DIRECTIVE
#ifdef SIZE_ASM_OP
#define ASM_OUTPUT_SIZE_DIRECTIVE(STREAM, NAME, SIZE)	\
  do							\
    {							\
      HOST_WIDE_INT size_ = (SIZE);			\
      fputs (SIZE_ASM_OP, STREAM);			\
      assemble_name (STREAM, NAME);			\
      fputs (", ", STREAM);				\
      fprintf (STREAM, HOST_WIDE_INT_PRINT_DEC, size_);	\
      putc ('\n', STREAM);				\
    }							\
  while (0)

#define ASM_OUTPUT_MEASURED_SIZE(STREAM, NAME)		\
  do							\
    {							\
      fputs (SIZE_ASM_OP, STREAM);			\
      assemble_name (STREAM, NAME);			\
      fputs (", .-", STREAM);				\
      assemble_name (STREAM, NAME);			\
      putc ('\n', STREAM);				\
    }							\
  while (0)

#endif
#endif

/* This determines whether or not we support weak symbols.  */
#ifndef SUPPORTS_WEAK
#if defined (ASM_WEAKEN_LABEL) || defined (ASM_WEAKEN_DECL)
#define SUPPORTS_WEAK 1
#else
#define SUPPORTS_WEAK 0
#endif
#endif

/* This determines whether or not we support link-once semantics.  */
#ifndef SUPPORTS_ONE_ONLY
#ifdef MAKE_DECL_ONE_ONLY
#define SUPPORTS_ONE_ONLY 1
#else
#define SUPPORTS_ONE_ONLY 0
#endif
#endif

/* By default, there is no prefix on user-defined symbols.  */
#ifndef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""
#endif

/* If the target supports weak symbols, define TARGET_ATTRIBUTE_WEAK to
   provide a weak attribute.  Else define it to nothing. 

   This would normally belong in ansidecl.h, but SUPPORTS_WEAK is
   not available at that time.

   Note, this is only for use by target files which we know are to be
   compiled by GCC.  */
#ifndef TARGET_ATTRIBUTE_WEAK
# if SUPPORTS_WEAK
#  define TARGET_ATTRIBUTE_WEAK __attribute__ ((weak))
# else
#  define TARGET_ATTRIBUTE_WEAK
# endif
#endif

/* If the target supports init_priority C++ attribute, give
   SUPPORTS_INIT_PRIORITY a nonzero value.  */
#ifndef SUPPORTS_INIT_PRIORITY
#define SUPPORTS_INIT_PRIORITY 1
#endif /* SUPPORTS_INIT_PRIORITY */

/* If duplicate library search directories can be removed from a
   linker command without changing the linker's semantics, give this
   symbol a nonzero.  */
#ifndef LINK_ELIMINATE_DUPLICATE_LDIRECTORIES
#define LINK_ELIMINATE_DUPLICATE_LDIRECTORIES 0
#endif /* LINK_ELIMINATE_DUPLICATE_LDIRECTORIES */

/* If we have a definition of INCOMING_RETURN_ADDR_RTX, assume that
   the rest of the DWARF 2 frame unwind support is also provided.  */
#if !defined (DWARF2_UNWIND_INFO) && defined (INCOMING_RETURN_ADDR_RTX)
#define DWARF2_UNWIND_INFO 1
#endif

/* If we have named sections, and we're using crtstuff to run ctors,
   use them for registering eh frame information.  */
#if defined (TARGET_ASM_NAMED_SECTION) && DWARF2_UNWIND_INFO \
    && !defined(EH_FRAME_IN_DATA_SECTION)
#ifndef EH_FRAME_SECTION_NAME
#define EH_FRAME_SECTION_NAME ".eh_frame"
#endif
#endif

/* If we have named section and we support weak symbols, then use the
   .jcr section for recording java classes which need to be registered
   at program start-up time.  */
#if defined (TARGET_ASM_NAMED_SECTION) && SUPPORTS_WEAK
#ifndef JCR_SECTION_NAME
#define JCR_SECTION_NAME ".jcr"
#endif
#endif

/* By default, we generate a label at the beginning and end of the
   text section, and compute the size of the text section by
   subtracting the two.  However, on some platforms that doesn't 
   work, and we use the section itself, rather than a label at the
   beginning of it, to indicate the start of the section.  On such
   platforms, define this to zero.  */
#ifndef DWARF2_GENERATE_TEXT_SECTION_LABEL
#define DWARF2_GENERATE_TEXT_SECTION_LABEL 1
#endif

/* Supply a default definition for PROMOTE_PROTOTYPES.  */
#ifndef PROMOTE_PROTOTYPES
#define PROMOTE_PROTOTYPES	0
#endif

/* Number of hardware registers that go into the DWARF-2 unwind info.
   If not defined, equals FIRST_PSEUDO_REGISTER  */

#ifndef DWARF_FRAME_REGISTERS
#define DWARF_FRAME_REGISTERS FIRST_PSEUDO_REGISTER
#endif

/* How to renumber registers for dbx and gdb.  If not defined, assume
   no renumbering is necessary.  */

#ifndef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)
#endif

/* Default sizes for base C types.  If the sizes are different for
   your target, you should override these values by defining the
   appropriate symbols in your tm.h file.  */

#ifndef BITS_PER_UNIT
#define BITS_PER_UNIT 8
#endif

#ifndef BITS_PER_WORD
#define BITS_PER_WORD (BITS_PER_UNIT * UNITS_PER_WORD)
#endif

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef BOOL_TYPE_SIZE
/* `bool' has size and alignment `1', on almost all platforms.  */
#define BOOL_TYPE_SIZE CHAR_TYPE_SIZE
#endif

#ifndef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE (BITS_PER_UNIT * MIN ((UNITS_PER_WORD + 1) / 2, 2))
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE INT_TYPE_SIZE
#endif

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

/* Width in bits of a pointer.  Mind the value of the macro `Pmode'.  */
#ifndef POINTER_SIZE
#define POINTER_SIZE BITS_PER_WORD
#endif

#ifndef BUILD_VA_LIST_TYPE
#define BUILD_VA_LIST_TYPE(X) ((X) = ptr_type_node)
#endif

#ifndef PIC_OFFSET_TABLE_REGNUM
#define PIC_OFFSET_TABLE_REGNUM INVALID_REGNUM
#endif

/* Type used by GCOV counters.  Use 64bit data type if target supports
   it.  */
#if LONG_TYPE_SIZE >= 64
#define GCOV_TYPE_SIZE LONG_TYPE_SIZE
#else
#define GCOV_TYPE_SIZE LONG_LONG_TYPE_SIZE
#endif


/* By default, the preprocessor should be invoked the same way in C++
   as in C.  */
#ifndef CPLUSPLUS_CPP_SPEC
#ifdef CPP_SPEC
#define CPLUSPLUS_CPP_SPEC CPP_SPEC
#endif
#endif

#ifndef ACCUMULATE_OUTGOING_ARGS
#define ACCUMULATE_OUTGOING_ARGS 0
#endif

/* Supply a default definition for PUSH_ARGS.  */
#ifndef PUSH_ARGS
#ifdef PUSH_ROUNDING
#define PUSH_ARGS	!ACCUMULATE_OUTGOING_ARGS
#else
#define PUSH_ARGS	0
#endif
#endif

/* If PREFERRED_STACK_BOUNDARY is not defined, set it to STACK_BOUNDARY.
   STACK_BOUNDARY is required.  */
#ifndef PREFERRED_STACK_BOUNDARY
#define PREFERRED_STACK_BOUNDARY STACK_BOUNDARY
#endif

/* By default, the C++ compiler will use function addresses in the
   vtable entries.  Setting this nonzero tells the compiler to use
   function descriptors instead.  The value of this macro says how
   many words wide the descriptor is (normally 2).  It is assumed 
   that the address of a function descriptor may be treated as a
   pointer to a function.  */
#ifndef TARGET_VTABLE_USES_DESCRIPTORS
#define TARGET_VTABLE_USES_DESCRIPTORS 0
#endif

/* By default, the vtable entries are void pointers, the so the alignment
   is the same as pointer alignment.  The value of this macro specifies
   the alignment of the vtable entry in bits.  It should be defined only
   when special alignment is necessary.  */
#ifndef TARGET_VTABLE_ENTRY_ALIGN
#define TARGET_VTABLE_ENTRY_ALIGN POINTER_SIZE
#endif

/* There are a few non-descriptor entries in the vtable at offsets below
   zero.  If these entries must be padded (say, to preserve the alignment
   specified by TARGET_VTABLE_ENTRY_ALIGN), set this to the number of
   words in each data entry.  */
#ifndef TARGET_VTABLE_DATA_ENTRY_DISTANCE
#define TARGET_VTABLE_DATA_ENTRY_DISTANCE 1
#endif

/* Select a format to encode pointers in exception handling data.  We
   prefer those that result in fewer dynamic relocations.  Assume no
   special support here and encode direct references.  */
#ifndef ASM_PREFERRED_EH_DATA_FORMAT
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)  DW_EH_PE_absptr
#endif

/* By default, the C++ compiler will use the lowest bit of the pointer
   to function to indicate a pointer-to-member-function points to a
   virtual member function.  However, if FUNCTION_BOUNDARY indicates
   function addresses aren't always even, the lowest bit of the delta
   field will be used.  */
#ifndef TARGET_PTRMEMFUNC_VBIT_LOCATION
#define TARGET_PTRMEMFUNC_VBIT_LOCATION \
  (FUNCTION_BOUNDARY >= 2 * BITS_PER_UNIT \
   ? ptrmemfunc_vbit_in_pfn : ptrmemfunc_vbit_in_delta)
#endif

#ifndef DEFAULT_GDB_EXTENSIONS
#define DEFAULT_GDB_EXTENSIONS 1
#endif

/* If more than one debugging type is supported, you must define
   PREFERRED_DEBUGGING_TYPE to choose a format in a system-dependent way.

   This is one long line cause VAXC can't handle a \-newline.  */
#if 1 < (defined (DBX_DEBUGGING_INFO) + defined (SDB_DEBUGGING_INFO) + defined (DWARF_DEBUGGING_INFO) + defined (DWARF2_DEBUGGING_INFO) + defined (XCOFF_DEBUGGING_INFO) + defined (VMS_DEBUGGING_INFO))
#ifndef PREFERRED_DEBUGGING_TYPE
You Lose!  You must define PREFERRED_DEBUGGING_TYPE!
#endif /* no PREFERRED_DEBUGGING_TYPE */
#else /* Only one debugging format supported.  Define PREFERRED_DEBUGGING_TYPE
	 so other code needn't care.  */
#ifdef DBX_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
#endif
#ifdef SDB_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE SDB_DEBUG
#endif
#ifdef DWARF_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DWARF_DEBUG
#endif
#ifdef DWARF2_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#endif
#ifdef VMS_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE VMS_AND_DWARF2_DEBUG
#endif
#ifdef XCOFF_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE XCOFF_DEBUG
#endif
#endif /* More than one debugger format enabled.  */

/* If still not defined, must have been because no debugging formats
   are supported.  */
#ifndef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE NO_DEBUG
#endif

/* Define codes for all the float formats that we know of.  */
#define UNKNOWN_FLOAT_FORMAT 0
#define IEEE_FLOAT_FORMAT 1
#define VAX_FLOAT_FORMAT 2
#define IBM_FLOAT_FORMAT 3
#define C4X_FLOAT_FORMAT 4

/* Default to IEEE float if not specified.  Nearly all machines use it.  */
#ifndef TARGET_FLOAT_FORMAT
#define	TARGET_FLOAT_FORMAT	IEEE_FLOAT_FORMAT
#endif

/* Determine the register class for registers suitable to be the base
   address register in a MEM.  Allow the choice to be dependent upon
   the mode of the memory access.  */
#ifndef MODE_BASE_REG_CLASS
#define MODE_BASE_REG_CLASS(MODE) BASE_REG_CLASS
#endif

#ifndef LARGEST_EXPONENT_IS_NORMAL
#define LARGEST_EXPONENT_IS_NORMAL(SIZE) 0
#endif

#ifndef ROUND_TOWARDS_ZERO
#define ROUND_TOWARDS_ZERO 0
#endif

#ifndef MODE_HAS_NANS
#define MODE_HAS_NANS(MODE)					\
  (FLOAT_MODE_P (MODE)						\
   && TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT			\
   && !LARGEST_EXPONENT_IS_NORMAL (GET_MODE_BITSIZE (MODE)))
#endif

#ifndef MODE_HAS_INFINITIES
#define MODE_HAS_INFINITIES(MODE)				\
  (FLOAT_MODE_P (MODE)						\
   && TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT			\
   && !LARGEST_EXPONENT_IS_NORMAL (GET_MODE_BITSIZE (MODE)))
#endif

#ifndef MODE_HAS_SIGNED_ZEROS
#define MODE_HAS_SIGNED_ZEROS(MODE) \
  (FLOAT_MODE_P (MODE) && TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT)
#endif

#ifndef MODE_HAS_SIGN_DEPENDENT_ROUNDING
#define MODE_HAS_SIGN_DEPENDENT_ROUNDING(MODE)			\
  (FLOAT_MODE_P (MODE)						\
   && TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT			\
   && !ROUND_TOWARDS_ZERO)
#endif

/* If FLOAT_WORDS_BIG_ENDIAN and HOST_FLOAT_WORDS_BIG_ENDIAN are not defined
   in the header files, then this implies the word-endianness is the same as
   for integers.  */
#ifndef FLOAT_WORDS_BIG_ENDIAN
#define FLOAT_WORDS_BIG_ENDIAN WORDS_BIG_ENDIAN
#endif

#ifndef TARGET_FLT_EVAL_METHOD
#define TARGET_FLT_EVAL_METHOD 0
#endif

#ifndef HOT_TEXT_SECTION_NAME
#define HOT_TEXT_SECTION_NAME "text.hot"
#endif

#ifndef UNLIKELY_EXECUTED_TEXT_SECTION_NAME
#define UNLIKELY_EXECUTED_TEXT_SECTION_NAME "text.unlikely"
#endif

#ifndef VECTOR_MODE_SUPPORTED_P
#define VECTOR_MODE_SUPPORTED_P(MODE) 0
#endif

/* Determine whether __cxa_atexit, rather than atexit, is used to
   register C++ destructors for local statics and global objects.  */
#ifndef DEFAULT_USE_CXA_ATEXIT
#define DEFAULT_USE_CXA_ATEXIT 0
#endif

/* Determine whether extra constraint letter should be handled
   via address reload (like 'o').  */
#ifndef EXTRA_MEMORY_CONSTRAINT
#define EXTRA_MEMORY_CONSTRAINT(C) 0
#endif

/* Determine whether extra constraint letter should be handled
   as an address (like 'p').  */
#ifndef EXTRA_ADDRESS_CONSTRAINT
#define EXTRA_ADDRESS_CONSTRAINT(C) 0
#endif

#endif  /* ! GCC_DEFAULTS_H */
