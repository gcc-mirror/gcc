/* Default initializers for a generic GCC target.
   Copyright (C) 2001 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

/* See target.h for a desciption of what this file contains and how to
   use it.

   We want to have non-NULL default definitions of all hook functions,
   even if they do nothing.  */

/* Assembler output.  */
#define TARGET_ASM_OPEN_PAREN "("
#define TARGET_ASM_CLOSE_PAREN ")"
#define TARGET_ASM_FUNCTION_PROLOGUE default_function_pro_epilogue
#define TARGET_ASM_FUNCTION_EPILOGUE default_function_pro_epilogue
#define TARGET_ASM_FUNCTION_END_PROLOGUE no_asm_to_stream
#define TARGET_ASM_FUNCTION_BEGIN_EPILOGUE no_asm_to_stream

#if !defined(TARGET_ASM_CONSTRUCTOR) && !defined(USE_COLLECT2)
# ifdef CTORS_SECTION_ASM_OP
#  define TARGET_ASM_CONSTRUCTOR default_ctor_section_asm_out_constructor
# else
#  ifdef TARGET_ASM_NAMED_SECTION
#   define TARGET_ASM_CONSTRUCTOR default_named_section_asm_out_constructor
#  else
#   define TARGET_ASM_CONSTRUCTOR default_stabs_asm_out_constructor
#  endif
# endif
#endif

#if !defined(TARGET_ASM_DESTRUCTOR) && !defined(USE_COLLECT2)
# ifdef DTORS_SECTION_ASM_OP
#  define TARGET_ASM_DESTRUCTOR default_dtor_section_asm_out_destructor
# else
#  ifdef TARGET_ASM_NAMED_SECTION
#   define TARGET_ASM_DESTRUCTOR default_named_section_asm_out_destructor
#  else
#   define TARGET_ASM_DESTRUCTOR default_stabs_asm_out_destructor
#  endif
# endif
#endif

#if defined(TARGET_ASM_CONSTRUCTOR) && defined(TARGET_ASM_DESTRUCTOR)
#define TARGET_HAVE_CTORS_DTORS true
#else
#define TARGET_HAVE_CTORS_DTORS false
#define TARGET_ASM_CONSTRUCTOR NULL
#define TARGET_ASM_DESTRUCTOR NULL
#endif

#ifdef TARGET_ASM_NAMED_SECTION
#define TARGET_HAVE_NAMED_SECTIONS true
#else
#define TARGET_ASM_NAMED_SECTION default_no_named_section
#define TARGET_HAVE_NAMED_SECTIONS false
#endif

#define TARGET_ASM_OUT {TARGET_ASM_OPEN_PAREN,			\
			TARGET_ASM_CLOSE_PAREN,			\
			TARGET_ASM_FUNCTION_PROLOGUE,		\
			TARGET_ASM_FUNCTION_END_PROLOGUE,	\
			TARGET_ASM_FUNCTION_BEGIN_EPILOGUE,	\
			TARGET_ASM_FUNCTION_EPILOGUE,		\
			TARGET_ASM_NAMED_SECTION,		\
			TARGET_ASM_CONSTRUCTOR,			\
			TARGET_ASM_DESTRUCTOR}

/* All in tree.c.  */
#define TARGET_MERGE_DECL_ATTRIBUTES merge_decl_attributes
#define TARGET_MERGE_TYPE_ATTRIBUTES merge_type_attributes
#define TARGET_VALID_DECL_ATTRIBUTE default_valid_attribute_p
#define TARGET_VALID_TYPE_ATTRIBUTE default_valid_attribute_p
#define TARGET_COMP_TYPE_ATTRIBUTES default_comp_type_attributes
#define TARGET_SET_DEFAULT_TYPE_ATTRIBUTES default_set_default_type_attributes
#define TARGET_INSERT_ATTRIBUTES default_insert_attributes

/* In builtins.c.  */
#define TARGET_INIT_BUILTINS default_init_builtins
#define TARGET_EXPAND_BUILTIN default_expand_builtin

/* In varasm.c.  */
#ifndef TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS default_section_type_flags
#endif

/* The whole shebang.  */
#define TARGET_INITIALIZER			\
{						\
  TARGET_ASM_OUT,				\
  TARGET_MERGE_DECL_ATTRIBUTES,			\
  TARGET_MERGE_TYPE_ATTRIBUTES,			\
  TARGET_VALID_DECL_ATTRIBUTE,			\
  TARGET_VALID_TYPE_ATTRIBUTE,			\
  TARGET_COMP_TYPE_ATTRIBUTES,			\
  TARGET_SET_DEFAULT_TYPE_ATTRIBUTES,		\
  TARGET_INSERT_ATTRIBUTES,			\
  TARGET_INIT_BUILTINS,				\
  TARGET_EXPAND_BUILTIN,			\
  TARGET_SECTION_TYPE_FLAGS,			\
  TARGET_HAVE_NAMED_SECTIONS,			\
  TARGET_HAVE_CTORS_DTORS			\
}
