/* Configuration for an i386 running MS-DOS with djgpp/go32.  */

/* Don't assume anything about the header files. */
#define NO_IMPLICIT_EXTERN_C

#define HANDLE_SYSV_PRAGMA

#define YES_UNDERSCORES

#include "i386/gas.h"

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   This only happens if the function declaration has the STDCALL attribute and
   the number of arguments is not variable */

#undef RETURN_POPS_ARGS
#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) \
  ( \
   TREE_CODE (FUNTYPE) == IDENTIFIER_NODE \
   ? \
     0 \
   : \
     ( \
      ((FUNDECL && (TREE_CODE_CLASS (TREE_CODE (FUNDECL)) == 'd') \
	? \
          lookup_attribute ("stdcall", \
			    DECL_MACHINE_ATTRIBUTES (FUNDECL)) != NULL_TREE \
	: 0 \
       ) \
      ) \
      && \
         ( \
          TYPE_ARG_TYPES (FUNTYPE) == 0 \
          || \
            TREE_VALUE (tree_last (TYPE_ARG_TYPES (FUNTYPE))) \
            == void_type_node \
         ) \
     ) \
     ? \
       (SIZE) \
     : \
       (aggregate_value_p (TREE_TYPE (FUNTYPE))) \
       ? \
         GET_MODE_SIZE (Pmode) \
       : \
         0 \
  )

/* Value is 1 if the declaration has either of the attributes: CDECL or
   STDCALL and 0 otherwise */

#define VALID_MACHINE_DECL_ATTRIBUTE(DECL, ATTR, NAME, ARGS) \
  (((TREE_CODE(DECL) == FUNCTION_DECL) \
    || (TREE_CODE(DECL) == FIELD_DECL) \
    || (TREE_CODE(DECL) == TYPE_DECL)) \
   && (is_attribute_p ("stdcall", (NAME)) \
       || is_attribute_p ("cdecl", (NAME))) \
   && (ARGS) == NULL)

#ifdef CPP_PREDEFINES
#undef CPP_PREDEFINES
#endif
#define CPP_PREDEFINES "-Dunix -Di386 -DGO32 -DMSDOS \
  -Asystem(unix) -Asystem(msdos) -Acpu(i386) -Amachine(i386)"

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_ctor, in_dtor

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS					\
  CTOR_SECTION_FUNCTION						\
  DTOR_SECTION_FUNCTION

#define CTOR_SECTION_FUNCTION					\
void								\
ctor_section ()							\
{								\
  if (in_section != in_ctor)					\
    {								\
      fprintf (asm_out_file, "\t.section .ctor\n");		\
      in_section = in_ctor;					\
    }								\
}

#define DTOR_SECTION_FUNCTION					\
void								\
dtor_section ()							\
{								\
  if (in_section != in_dtor)					\
    {								\
      fprintf (asm_out_file, "\t.section .dtor\n");		\
      in_section = in_dtor;					\
    }								\
}

#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)	\
  do {						\
    ctor_section ();				\
    fprintf (FILE, "%s\t", ASM_LONG);		\
    assemble_name (FILE, NAME);			\
    fprintf (FILE, "\n");			\
  } while (0)

#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       	\
  do {						\
    dtor_section ();                   		\
    fprintf (FILE, "%s\t", ASM_LONG);		\
    assemble_name (FILE, NAME);              	\
    fprintf (FILE, "\n");			\
  } while (0)


