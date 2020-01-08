/* Subroutines used for ISR of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2020 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* ------------------------------------------------------------------------ */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "diagnostic-core.h"
#include "output.h"

/* ------------------------------------------------------------------------ */

/* Refer to nds32.h, there are maximum 73 isr vectors in nds32 architecture.
   0 for reset handler with __attribute__((reset())),
   1-8 for exception handler with __attribute__((exception(1,...,8))),
   and 9-72 for interrupt handler with __attribute__((interrupt(0,...,63))).
   We use an array to record essential information for each vector.  */
static struct nds32_isr_info nds32_isr_vectors[NDS32_N_ISR_VECTORS];

/* ------------------------------------------------------------- */
/* FIXME:
   FOR BACKWARD COMPATIBILITY, we need to support following patterns:

       __attribute__((interrupt("XXX;YYY;id=ZZZ")))
       __attribute__((exception("XXX;YYY;id=ZZZ")))
       __attribute__((reset("vectors=XXX;nmi_func=YYY;warm_func=ZZZ")))

   We provide several functions to parse the strings.  */

static void
nds32_interrupt_attribute_parse_string (const char *original_str,
					const char *func_name,
					unsigned int s_level)
{
  char target_str[100];
  enum nds32_isr_save_reg save_reg;
  enum nds32_isr_nested_type nested_type;

  char *save_all_regs_str, *save_caller_regs_str;
  char *nested_str, *not_nested_str, *ready_nested_str, *critical_str;
  char *id_str, *value_str;

  /* Copy original string into a character array so that
     the string APIs can handle it.  */
  strcpy (target_str, original_str);

  /* 1. Detect 'save_all_regs'    : NDS32_SAVE_ALL
	       'save_caller_regs' : NDS32_PARTIAL_SAVE */
  save_all_regs_str    = strstr (target_str, "save_all_regs");
  save_caller_regs_str = strstr (target_str, "save_caller_regs");

  /* Note that if no argument is found,
     use NDS32_PARTIAL_SAVE by default.  */
  if (save_all_regs_str)
    save_reg = NDS32_SAVE_ALL;
  else if (save_caller_regs_str)
    save_reg = NDS32_PARTIAL_SAVE;
  else
    save_reg = NDS32_PARTIAL_SAVE;

  /* 2. Detect 'nested'       : NDS32_NESTED
	       'not_nested'   : NDS32_NOT_NESTED
	       'ready_nested' : NDS32_NESTED_READY
	       'critical'     : NDS32_CRITICAL */
  nested_str       = strstr (target_str, "nested");
  not_nested_str   = strstr (target_str, "not_nested");
  ready_nested_str = strstr (target_str, "ready_nested");
  critical_str     = strstr (target_str, "critical");

  /* Note that if no argument is found,
     use NDS32_NOT_NESTED by default.
     Also, since 'not_nested' and 'ready_nested' both contains
     'nested' string, we check 'nested' with lowest priority.  */
  if (not_nested_str)
    nested_type = NDS32_NOT_NESTED;
  else if (ready_nested_str)
    nested_type = NDS32_NESTED_READY;
  else if (nested_str)
    nested_type = NDS32_NESTED;
  else if (critical_str)
    nested_type = NDS32_CRITICAL;
  else
    nested_type = NDS32_NOT_NESTED;

  /* 3. Traverse each id value and set corresponding information.  */
  id_str = strstr (target_str, "id=");

  /* If user forgets to assign 'id', issue an error message.  */
  if (id_str == NULL)
    error ("require id argument in the string");
  /* Extract the value_str first.  */
  id_str    = strtok (id_str, "=");
  value_str = strtok (NULL, ";");

  /* Pick up the first id value token.  */
  value_str = strtok (value_str, ",");
  while (value_str != NULL)
    {
      int i;
      i = atoi (value_str);

      /* For interrupt(0..63), the actual vector number is (9..72).  */
      i = i + 9;
      if (i < 9 || i > 72)
	error ("invalid id value for interrupt attribute");

      /* Setup nds32_isr_vectors[] array.  */
      nds32_isr_vectors[i].category = NDS32_ISR_INTERRUPT;
      strcpy (nds32_isr_vectors[i].func_name, func_name);
      nds32_isr_vectors[i].save_reg = save_reg;
      nds32_isr_vectors[i].nested_type = nested_type;
      nds32_isr_vectors[i].security_level = s_level;

      /* Fetch next token.  */
      value_str = strtok (NULL, ",");
    }

  return;
}

static void
nds32_exception_attribute_parse_string (const char *original_str,
					const char *func_name,
					unsigned int s_level)
{
  char target_str[100];
  enum nds32_isr_save_reg save_reg;
  enum nds32_isr_nested_type nested_type;

  char *save_all_regs_str, *save_caller_regs_str;
  char *nested_str, *not_nested_str, *ready_nested_str, *critical_str;
  char *id_str, *value_str;

  /* Copy original string into a character array so that
     the string APIs can handle it.  */
  strcpy (target_str, original_str);

  /* 1. Detect 'save_all_regs'    : NDS32_SAVE_ALL
	       'save_caller_regs' : NDS32_PARTIAL_SAVE */
  save_all_regs_str    = strstr (target_str, "save_all_regs");
  save_caller_regs_str = strstr (target_str, "save_caller_regs");

  /* Note that if no argument is found,
     use NDS32_PARTIAL_SAVE by default.  */
  if (save_all_regs_str)
    save_reg = NDS32_SAVE_ALL;
  else if (save_caller_regs_str)
    save_reg = NDS32_PARTIAL_SAVE;
  else
    save_reg = NDS32_PARTIAL_SAVE;

  /* 2. Detect 'nested'       : NDS32_NESTED
	       'not_nested'   : NDS32_NOT_NESTED
	       'ready_nested' : NDS32_NESTED_READY
	       'critical'     : NDS32_CRITICAL */
  nested_str       = strstr (target_str, "nested");
  not_nested_str   = strstr (target_str, "not_nested");
  ready_nested_str = strstr (target_str, "ready_nested");
  critical_str     = strstr (target_str, "critical");

  /* Note that if no argument is found,
     use NDS32_NOT_NESTED by default.
     Also, since 'not_nested' and 'ready_nested' both contains
     'nested' string, we check 'nested' with lowest priority.  */
  if (not_nested_str)
    nested_type = NDS32_NOT_NESTED;
  else if (ready_nested_str)
    nested_type = NDS32_NESTED_READY;
  else if (nested_str)
    nested_type = NDS32_NESTED;
  else if (critical_str)
    nested_type = NDS32_CRITICAL;
  else
    nested_type = NDS32_NOT_NESTED;

  /* 3. Traverse each id value and set corresponding information.  */
  id_str = strstr (target_str, "id=");

  /* If user forgets to assign 'id', issue an error message.  */
  if (id_str == NULL)
    error ("require id argument in the string");
  /* Extract the value_str first.  */
  id_str    = strtok (id_str, "=");
  value_str = strtok (NULL, ";");

  /* Pick up the first id value token.  */
  value_str = strtok (value_str, ",");
  while (value_str != NULL)
    {
      int i;
      i = atoi (value_str);

      /* For exception(1..8), the actual vector number is (1..8).  */
      if (i < 1 || i > 8)
	error ("invalid id value for exception attribute");

      /* Setup nds32_isr_vectors[] array.  */
      nds32_isr_vectors[i].category = NDS32_ISR_EXCEPTION;
      strcpy (nds32_isr_vectors[i].func_name, func_name);
      nds32_isr_vectors[i].save_reg = save_reg;
      nds32_isr_vectors[i].nested_type = nested_type;
      nds32_isr_vectors[i].security_level = s_level;

      /* Fetch next token.  */
      value_str = strtok (NULL, ",");
    }

  return;
}

static void
nds32_reset_attribute_parse_string (const char *original_str,
				    const char *func_name)
{
  char target_str[100];
  char *vectors_str, *nmi_str, *warm_str, *value_str;

  /* Deal with reset attribute.  Its vector number is always 0.  */
  nds32_isr_vectors[0].category = NDS32_ISR_RESET;


  /* 1. Parse 'vectors=XXXX'.  */

  /* Copy original string into a character array so that
     the string APIs can handle it.  */
  strcpy (target_str, original_str);
  vectors_str = strstr (target_str, "vectors=");
  /* The total vectors = interrupt + exception numbers + reset.
     There are 8 exception and 1 reset in nds32 architecture.
     If user forgets to assign 'vectors', user default 16 interrupts.  */
  if (vectors_str != NULL)
    {
      /* Extract the value_str.  */
      vectors_str = strtok (vectors_str, "=");
      value_str  = strtok (NULL, ";");
      nds32_isr_vectors[0].total_n_vectors = atoi (value_str) + 8 + 1;
    }
  else
    nds32_isr_vectors[0].total_n_vectors = 16 + 8 + 1;
  strcpy (nds32_isr_vectors[0].func_name, func_name);


  /* 2. Parse 'nmi_func=YYYY'.  */

  /* Copy original string into a character array so that
     the string APIs can handle it.  */
  strcpy (target_str, original_str);
  nmi_str = strstr (target_str, "nmi_func=");
  if (nmi_str != NULL)
    {
      /* Extract the value_str.  */
      nmi_str = strtok (nmi_str, "=");
      value_str  = strtok (NULL, ";");
      strcpy (nds32_isr_vectors[0].nmi_name, value_str);
    }

  /* 3. Parse 'warm_func=ZZZZ'.  */

  /* Copy original string into a character array so that
     the string APIs can handle it.  */
  strcpy (target_str, original_str);
  warm_str = strstr (target_str, "warm_func=");
  if (warm_str != NULL)
    {
      /* Extract the value_str.  */
      warm_str = strtok (warm_str, "=");
      value_str  = strtok (NULL, ";");
      strcpy (nds32_isr_vectors[0].warm_name, value_str);
    }

  return;
}
/* ------------------------------------------------------------- */

/* A helper function to emit section head template.  */
static void
nds32_emit_section_head_template (char section_name[],
				  char symbol_name[],
				  int align_value,
				  bool object_p)
{
  const char *flags_str;
  const char *type_str;

  flags_str = (object_p) ? "\"a\"" : "\"ax\"";
  type_str = (object_p) ? "@object" : "@function";

  fprintf (asm_out_file, "\t.section\t%s, %s\n", section_name, flags_str);
  fprintf (asm_out_file, "\t.align\t%d\n", align_value);
  fprintf (asm_out_file, "\t.global\t%s\n", symbol_name);
  fprintf (asm_out_file, "\t.type\t%s, %s\n", symbol_name, type_str);
  fprintf (asm_out_file, "%s:\n", symbol_name);
}

/* A helper function to emit section tail template.  */
static void
nds32_emit_section_tail_template (char symbol_name[])
{
  fprintf (asm_out_file, "\t.size\t%s, .-%s\n", symbol_name, symbol_name);
}

/* Function to emit isr jump table section.  */
static void
nds32_emit_isr_jmptbl_section (int vector_id)
{
  char section_name[100];
  char symbol_name[100];

  /* A critical isr does not need jump table section because
     its behavior is not performed by two-level handler.  */
  if (nds32_isr_vectors[vector_id].nested_type == NDS32_CRITICAL)
    {
      fprintf (asm_out_file, "\t! The vector %02d is a critical isr !\n",
			     vector_id);
      return;
    }

  /* Prepare jmptbl section and symbol name.  */
  snprintf (section_name, sizeof (section_name),
	    ".nds32_jmptbl.%02d", vector_id);
  snprintf (symbol_name, sizeof (symbol_name),
	    "_nds32_jmptbl_%02d", vector_id);

  nds32_emit_section_head_template (section_name, symbol_name, 2, true);
  fprintf (asm_out_file, "\t.word\t%s\n",
			 nds32_isr_vectors[vector_id].func_name);
  nds32_emit_section_tail_template (symbol_name);
}

/* Function to emit isr vector section.  */
static void
nds32_emit_isr_vector_section (int vector_id)
{
  unsigned int vector_number_offset = 0;
  const char *c_str = "CATEGORY";
  const char *sr_str = "SR";
  const char *nt_str = "NT";
  char first_level_handler_name[100];
  char section_name[100];
  char symbol_name[100];

  /* Set the vector number offset so that we can calculate
     the value that user specifies in the attribute.
     We also prepare the category string for first level handler name.  */
  switch (nds32_isr_vectors[vector_id].category)
    {
    case NDS32_ISR_INTERRUPT:
      vector_number_offset = 9;
      c_str = "i";
      break;
    case NDS32_ISR_EXCEPTION:
      vector_number_offset = 0;
      c_str = "e";
      break;
    case NDS32_ISR_NONE:
    case NDS32_ISR_RESET:
      /* Normally it should not be here.  */
      gcc_unreachable ();
      break;
    }

  /* Prepare save reg string for first level handler name.  */
  switch (nds32_isr_vectors[vector_id].save_reg)
    {
    case NDS32_SAVE_ALL:
      sr_str = "sa";
      break;
    case NDS32_PARTIAL_SAVE:
      sr_str = "ps";
      break;
    }

  /* Prepare nested type string for first level handler name.  */
  switch (nds32_isr_vectors[vector_id].nested_type)
    {
    case NDS32_NESTED:
      nt_str = "ns";
      break;
    case NDS32_NOT_NESTED:
      nt_str = "nn";
      break;
    case NDS32_NESTED_READY:
      nt_str = "nr";
      break;
    case NDS32_CRITICAL:
      /* The critical isr is not performed by two-level handler.  */
      nt_str = "";
      break;
    }

  /* Now we can create first level handler name.  */
  if (nds32_isr_vectors[vector_id].security_level == 0)
    {
      /* For security level 0, use normal first level handler name.  */
      snprintf (first_level_handler_name, sizeof (first_level_handler_name),
		"_nds32_%s_%s_%s", c_str, sr_str, nt_str);
    }
  else
    {
      /* For security level 1-3, use corresponding spl_1, spl_2, or spl_3.  */
      snprintf (first_level_handler_name, sizeof (first_level_handler_name),
		"_nds32_spl_%d", nds32_isr_vectors[vector_id].security_level);
    }

  /* Prepare vector section and symbol name.  */
  snprintf (section_name, sizeof (section_name),
	    ".nds32_vector.%02d", vector_id);
  snprintf (symbol_name, sizeof (symbol_name),
	    "_nds32_vector_%02d", vector_id);


  /* Everything is ready.  We can start emit vector section content.  */
  nds32_emit_section_head_template (section_name, symbol_name,
				    floor_log2 (nds32_isr_vector_size), false);

  /* First we check if it is a critical isr.
     If so, jump to user handler directly; otherwise, the instructions
     in the vector section may be different according to the vector size.  */
  if (nds32_isr_vectors[vector_id].nested_type == NDS32_CRITICAL)
    {
      /* This block is for critical isr.  Jump to user handler directly.  */
      fprintf (asm_out_file, "\tj\t%s ! jump to user handler directly\n",
			     nds32_isr_vectors[vector_id].func_name);
    }
  else if (nds32_isr_vector_size == 4)
    {
      /* This block is for 4-byte vector size.
	 Hardware $VID support is necessary and only one instruction
	 is needed in vector section.  */
      fprintf (asm_out_file, "\tj\t%s ! jump to first level handler\n",
			     first_level_handler_name);
    }
  else
    {
      /* This block is for 16-byte vector size.
	 There is NO hardware $VID so that we need several instructions
	 such as pushing GPRs and preparing software vid at vector section.
	 For pushing GPRs, there are four variations for
	 16-byte vector content and we have to handle each combination.
	 For preparing software vid, note that the vid need to
	 be substracted vector_number_offset.  */
      if (TARGET_REDUCED_REGS)
	{
	  if (nds32_isr_vectors[vector_id].save_reg == NDS32_SAVE_ALL)
	    {
	      /* Case of reduced set registers and save_all attribute.  */
	      fprintf (asm_out_file, "\t! reduced set regs + save_all\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r15, [$sp], $r15, 0xf\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r0, [$sp], $r10, 0x0\n");

	    }
	  else
	    {
	      /* Case of reduced set registers and partial_save attribute.  */
	      fprintf (asm_out_file, "\t! reduced set regs + partial_save\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r15, [$sp], $r15, 0x2\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r0, [$sp], $r5, 0x0\n");
	    }
	}
      else
	{
	  if (nds32_isr_vectors[vector_id].save_reg == NDS32_SAVE_ALL)
	    {
	      /* Case of full set registers and save_all attribute.  */
	      fprintf (asm_out_file, "\t! full set regs + save_all\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r0, [$sp], $r27, 0xf\n");
	    }
	  else
	    {
	      /* Case of full set registers and partial_save attribute.  */
	      fprintf (asm_out_file, "\t! full set regs + partial_save\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r15, [$sp], $r27, 0x2\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r0, [$sp], $r5, 0x0\n");
	    }
	}

      fprintf (asm_out_file, "\tmovi\t$r0, %d ! preparing software vid\n",
			     vector_id - vector_number_offset);
      fprintf (asm_out_file, "\tj\t%s ! jump to first level handler\n",
			     first_level_handler_name);
    }

  nds32_emit_section_tail_template (symbol_name);
}

/* Function to emit isr reset handler content.
   Including all jmptbl/vector references, jmptbl section,
   vector section, nmi handler section, and warm handler section.  */
static void
nds32_emit_isr_reset_content (void)
{
  unsigned int i;
  unsigned int total_n_vectors;
  char reset_handler_name[100];
  char section_name[100];
  char symbol_name[100];

  total_n_vectors = nds32_isr_vectors[0].total_n_vectors;

  fprintf (asm_out_file, "\t! RESET HANDLER CONTENT - BEGIN !\n");

  /* Create references in .rodata according to total number of vectors.  */
  fprintf (asm_out_file, "\t.section\t.rodata\n");
  fprintf (asm_out_file, "\t.align\t2\n");

  /* Emit jmptbl references.  */
  fprintf (asm_out_file, "\t ! references to jmptbl section entries\n");
  for (i = 0; i < total_n_vectors; i++)
    fprintf (asm_out_file, "\t.word\t_nds32_jmptbl_%02d\n", i);

  /* Emit vector references.  */
  fprintf (asm_out_file, "\t ! references to vector section entries\n");
  for (i = 0; i < total_n_vectors; i++)
    fprintf (asm_out_file, "\t.word\t_nds32_vector_%02d\n", i);

  /* Emit jmptbl_00 section.  */
  snprintf (section_name, sizeof (section_name), ".nds32_jmptbl.00");
  snprintf (symbol_name, sizeof (symbol_name), "_nds32_jmptbl_00");

  fprintf (asm_out_file, "\t! ....................................\n");
  nds32_emit_section_head_template (section_name, symbol_name, 2, true);
  fprintf (asm_out_file, "\t.word\t%s\n",
			 nds32_isr_vectors[0].func_name);
  nds32_emit_section_tail_template (symbol_name);

  /* Emit vector_00 section.  */
  snprintf (section_name, sizeof (section_name), ".nds32_vector.00");
  snprintf (symbol_name, sizeof (symbol_name), "_nds32_vector_00");
  snprintf (reset_handler_name, sizeof (reset_handler_name),
	    "_nds32_reset");

  fprintf (asm_out_file, "\t! ....................................\n");
  nds32_emit_section_head_template (section_name, symbol_name,
				    floor_log2 (nds32_isr_vector_size), false);
  fprintf (asm_out_file, "\tj\t%s ! jump to reset handler\n",
			 reset_handler_name);
  nds32_emit_section_tail_template (symbol_name);

  /* Emit nmi handler section.  */
  snprintf (section_name, sizeof (section_name), ".nds32_nmih");
  snprintf (symbol_name, sizeof (symbol_name), "_nds32_nmih");

  fprintf (asm_out_file, "\t! ....................................\n");
  nds32_emit_section_head_template (section_name, symbol_name, 2, true);
  fprintf (asm_out_file, "\t.word\t%s\n",
			 (strlen (nds32_isr_vectors[0].nmi_name) == 0)
			 ? "0"
			 : nds32_isr_vectors[0].nmi_name);
  nds32_emit_section_tail_template (symbol_name);

  /* Emit warm handler section.  */
  snprintf (section_name, sizeof (section_name), ".nds32_wrh");
  snprintf (symbol_name, sizeof (symbol_name), "_nds32_wrh");

  fprintf (asm_out_file, "\t! ....................................\n");
  nds32_emit_section_head_template (section_name, symbol_name, 2, true);
  fprintf (asm_out_file, "\t.word\t%s\n",
			 (strlen (nds32_isr_vectors[0].warm_name) == 0)
			 ? "0"
			 : nds32_isr_vectors[0].warm_name);
  nds32_emit_section_tail_template (symbol_name);

  fprintf (asm_out_file, "\t! RESET HANDLER CONTENT - END !\n");
}

/* Function for nds32_merge_decl_attributes() and nds32_insert_attributes()
   to check if there are any conflict isr-specific attributes being set.
   We need to check:
     1. Only 'save_all' or 'partial_save' in the attributes.
     2. Only 'nested', 'not_nested', or 'nested_ready' in the attributes.
     3. Only 'interrupt', 'exception', or 'reset' in the attributes.  */
void
nds32_check_isr_attrs_conflict (tree func_decl, tree func_attrs)
{
  int save_all_p, partial_save_p;
  int nested_p, not_nested_p, nested_ready_p, critical_p;
  int intr_p, excp_p, reset_p;

  /* Initialize variables.  */
  save_all_p = partial_save_p = 0;
  nested_p = not_nested_p = nested_ready_p = critical_p = 0;
  intr_p = excp_p = reset_p = 0;

  /* We must check at MOST one attribute to set save-reg.  */
  if (lookup_attribute ("save_all", func_attrs))
    save_all_p = 1;
  if (lookup_attribute ("partial_save", func_attrs))
    partial_save_p = 1;

  if ((save_all_p + partial_save_p) > 1)
    error ("multiple save reg attributes to function %qD", func_decl);

  /* We must check at MOST one attribute to set nested-type.  */
  if (lookup_attribute ("nested", func_attrs))
    nested_p = 1;
  if (lookup_attribute ("not_nested", func_attrs))
    not_nested_p = 1;
  if (lookup_attribute ("nested_ready", func_attrs))
    nested_ready_p = 1;
  if (lookup_attribute ("critical", func_attrs))
    critical_p = 1;

  if ((nested_p + not_nested_p + nested_ready_p + critical_p) > 1)
    error ("multiple nested types attributes to function %qD", func_decl);

  /* We must check at MOST one attribute to
     set interrupt/exception/reset.  */
  if (lookup_attribute ("interrupt", func_attrs))
    intr_p = 1;
  if (lookup_attribute ("exception", func_attrs))
    excp_p = 1;
  if (lookup_attribute ("reset", func_attrs))
    reset_p = 1;

  if ((intr_p + excp_p + reset_p) > 1)
    error ("multiple interrupt attributes to function %qD", func_decl);

  /* Do not allow isr attributes under linux toolchain.  */
  if (TARGET_LINUX_ABI && intr_p)
      error ("cannot use interrupt attributes to function %qD "
	     "under linux toolchain", func_decl);
  if (TARGET_LINUX_ABI && excp_p)
      error ("cannot use exception attributes to function %qD "
	     "under linux toolchain", func_decl);
  if (TARGET_LINUX_ABI && reset_p)
      error ("cannot use reset attributes to function %qD "
	     "under linux toolchain", func_decl);
}

/* Function to construct isr vectors information array.
   We DO NOT HAVE TO check if the attributes are valid
   because those works are supposed to be done on
   nds32_merge_decl_attributes() and nds32_insert_attributes().  */
void
nds32_construct_isr_vectors_information (tree func_attrs,
					 const char *func_name)
{
  tree save_all, partial_save;
  tree nested, not_nested, nested_ready, critical;
  tree intr, excp, reset;

  tree secure;
  tree security_level_list;
  tree security_level;
  unsigned int s_level;

  save_all     = lookup_attribute ("save_all", func_attrs);
  partial_save = lookup_attribute ("partial_save", func_attrs);

  nested       = lookup_attribute ("nested", func_attrs);
  not_nested   = lookup_attribute ("not_nested", func_attrs);
  nested_ready = lookup_attribute ("nested_ready", func_attrs);
  critical     = lookup_attribute ("critical", func_attrs);

  intr  = lookup_attribute ("interrupt", func_attrs);
  excp  = lookup_attribute ("exception", func_attrs);
  reset = lookup_attribute ("reset", func_attrs);

  /* If there is no interrupt/exception/reset, we can return immediately.  */
  if (!intr && !excp && !reset)
    return;

  /* At first, we need to retrieve security level.  */
  secure = lookup_attribute ("secure", func_attrs);
  if (secure != NULL)
    {
      security_level_list = TREE_VALUE (secure);
      security_level = TREE_VALUE (security_level_list);
      s_level = TREE_INT_CST_LOW (security_level);
    }
  else
    {
      /* If there is no secure attribute, the security level is set by
	 nds32_isr_secure_level, which is controlled by -misr-secure=X option.
	 By default nds32_isr_secure_level should be 0.  */
      s_level = nds32_isr_secure_level;
    }

  /* ------------------------------------------------------------- */
  /* FIXME:
     FOR BACKWARD COMPATIBILITY, we need to support following patterns:

	 __attribute__((interrupt("XXX;YYY;id=ZZZ")))
	 __attribute__((exception("XXX;YYY;id=ZZZ")))
	 __attribute__((reset("vectors=XXX;nmi_func=YYY;warm_func=ZZZ")))

     If interrupt/exception/reset appears and its argument is a
     STRING_CST, we will parse string with some auxiliary functions
     which set necessary isr information in the nds32_isr_vectors[] array.
     After that, we can return immediately to avoid new-syntax isr
     information construction.  */
  if (intr != NULL_TREE
      && TREE_CODE (TREE_VALUE (TREE_VALUE (intr))) == STRING_CST)
    {
      tree string_arg = TREE_VALUE (TREE_VALUE (intr));
      nds32_interrupt_attribute_parse_string (TREE_STRING_POINTER (string_arg),
					      func_name,
					      s_level);
      return;
    }
  if (excp != NULL_TREE
      && TREE_CODE (TREE_VALUE (TREE_VALUE (excp))) == STRING_CST)
    {
      tree string_arg = TREE_VALUE (TREE_VALUE (excp));
      nds32_exception_attribute_parse_string (TREE_STRING_POINTER (string_arg),
					      func_name,
					      s_level);
      return;
    }
  if (reset != NULL_TREE
      && TREE_CODE (TREE_VALUE (TREE_VALUE (reset))) == STRING_CST)
    {
      tree string_arg = TREE_VALUE (TREE_VALUE (reset));
      nds32_reset_attribute_parse_string (TREE_STRING_POINTER (string_arg),
					  func_name);
      return;
    }
  /* ------------------------------------------------------------- */

  /* If we are here, either we have interrupt/exception,
     or reset attribute.  */
  if (intr || excp)
    {
      tree id_list;

      /* Prepare id list so that we can traverse and set vector id.  */
      id_list = (intr) ? (TREE_VALUE (intr)) : (TREE_VALUE (excp));

      while (id_list)
	{
	  tree id;
	  int vector_id;
	  unsigned int vector_number_offset;

	  /* The way to handle interrupt or exception is the same,
	     we just need to take care of actual vector number.
	     For interrupt(0..63), the actual vector number is (9..72).
	     For exception(1..8), the actual vector number is (1..8).  */
	  vector_number_offset = (intr) ? (9) : (0);

	  /* Pick up each vector id value.  */
	  id = TREE_VALUE (id_list);
	  /* Add vector_number_offset to get actual vector number.  */
	  vector_id = TREE_INT_CST_LOW (id) + vector_number_offset;

	  /* Set security level.  */
	  nds32_isr_vectors[vector_id].security_level = s_level;

	  /* Enable corresponding vector and set function name.  */
	  nds32_isr_vectors[vector_id].category = (intr)
						  ? (NDS32_ISR_INTERRUPT)
						  : (NDS32_ISR_EXCEPTION);
	  strcpy (nds32_isr_vectors[vector_id].func_name, func_name);

	  /* Set register saving scheme.  */
	  if (save_all)
	    nds32_isr_vectors[vector_id].save_reg = NDS32_SAVE_ALL;
	  else if (partial_save)
	    nds32_isr_vectors[vector_id].save_reg = NDS32_PARTIAL_SAVE;

	  /* Set nested type.  */
	  if (nested)
	    nds32_isr_vectors[vector_id].nested_type = NDS32_NESTED;
	  else if (not_nested)
	    nds32_isr_vectors[vector_id].nested_type = NDS32_NOT_NESTED;
	  else if (nested_ready)
	    nds32_isr_vectors[vector_id].nested_type = NDS32_NESTED_READY;
	  else if (critical)
	    nds32_isr_vectors[vector_id].nested_type = NDS32_CRITICAL;

	  /* Advance to next id.  */
	  id_list = TREE_CHAIN (id_list);
	}
    }
  else
    {
      tree id_list;
      tree id;
      tree nmi, warm;

      /* Deal with reset attribute.  Its vector number is always 0.  */
      nds32_isr_vectors[0].category = NDS32_ISR_RESET;

      /* Prepare id_list and identify id value so that
	 we can set total number of vectors.  */
      id_list = TREE_VALUE (reset);
      id = TREE_VALUE (id_list);

      /* The total vectors = interrupt + exception numbers + reset.
	 There are 8 exception and 1 reset in nds32 architecture.  */
      nds32_isr_vectors[0].total_n_vectors = TREE_INT_CST_LOW (id) + 8 + 1;
      strcpy (nds32_isr_vectors[0].func_name, func_name);

      /* Retrieve nmi and warm function.  */
      nmi  = lookup_attribute ("nmi", func_attrs);
      warm = lookup_attribute ("warm", func_attrs);

      if (nmi != NULL_TREE)
	{
	  tree nmi_func_list;
	  tree nmi_func;

	  nmi_func_list = TREE_VALUE (nmi);
	  nmi_func = TREE_VALUE (nmi_func_list);

	  /* Record nmi function name.  */
	  strcpy (nds32_isr_vectors[0].nmi_name,
		  IDENTIFIER_POINTER (nmi_func));
	}

      if (warm != NULL_TREE)
	{
	  tree warm_func_list;
	  tree warm_func;

	  warm_func_list = TREE_VALUE (warm);
	  warm_func = TREE_VALUE (warm_func_list);

	  /* Record warm function name.  */
	  strcpy (nds32_isr_vectors[0].warm_name,
		  IDENTIFIER_POINTER (warm_func));
	}
    }
}

void
nds32_asm_file_start_for_isr (void)
{
  int i;

  /* Initialize isr vector information array before compiling functions.  */
  for (i = 0; i < NDS32_N_ISR_VECTORS; i++)
    {
      nds32_isr_vectors[i].category = NDS32_ISR_NONE;
      strcpy (nds32_isr_vectors[i].func_name, "");
      nds32_isr_vectors[i].save_reg = NDS32_PARTIAL_SAVE;
      nds32_isr_vectors[i].nested_type = NDS32_NOT_NESTED;
      nds32_isr_vectors[i].security_level = 0;
      nds32_isr_vectors[i].total_n_vectors = 0;
      strcpy (nds32_isr_vectors[i].nmi_name, "");
      strcpy (nds32_isr_vectors[i].warm_name, "");
    }
}

void nds32_asm_file_end_for_isr (void)
{
  int i;

  /* If all the vectors are NDS32_ISR_NONE, we can return immediately.  */
  for (i = 0; i < NDS32_N_ISR_VECTORS; i++)
    if (nds32_isr_vectors[i].category != NDS32_ISR_NONE)
      break;

  if (i == NDS32_N_ISR_VECTORS)
    return;

  /* At least one vector is NOT NDS32_ISR_NONE,
     we should output isr vector information.  */
  fprintf (asm_out_file, "\t! ------------------------------------\n");
  fprintf (asm_out_file, "\t! The isr vector information:\n");
  fprintf (asm_out_file, "\t! ------------------------------------\n");

  /* Check reset handler first.  Its vector number is always 0.  */
  if (nds32_isr_vectors[0].category == NDS32_ISR_RESET)
    {
      nds32_emit_isr_reset_content ();
      fprintf (asm_out_file, "\t! ------------------------------------\n");
    }

  /* Check other vectors, starting from vector number 1.  */
  for (i = 1; i < NDS32_N_ISR_VECTORS; i++)
    {
      if (nds32_isr_vectors[i].category == NDS32_ISR_INTERRUPT
	  || nds32_isr_vectors[i].category == NDS32_ISR_EXCEPTION)
	{
	  /* Found one vector which is interupt or exception.
	     Output its jmptbl and vector section content.  */
	  fprintf (asm_out_file, "\t! interrupt/exception vector %02d\n", i);
	  fprintf (asm_out_file, "\t! security level: %d\n",
		   nds32_isr_vectors[i].security_level);
	  fprintf (asm_out_file, "\t! ------------------------------------\n");
	  nds32_emit_isr_jmptbl_section (i);
	  fprintf (asm_out_file, "\t! ....................................\n");
	  nds32_emit_isr_vector_section (i);
	  fprintf (asm_out_file, "\t! ------------------------------------\n");
	}
    }
}

/* Return true if FUNC is a isr function.  */
bool
nds32_isr_function_p (tree func)
{
  tree t_intr;
  tree t_excp;
  tree t_reset;

  tree attrs;

  if (TREE_CODE (func) != FUNCTION_DECL)
    abort ();

  attrs = DECL_ATTRIBUTES (func);

  t_intr  = lookup_attribute ("interrupt", attrs);
  t_excp  = lookup_attribute ("exception", attrs);
  t_reset = lookup_attribute ("reset", attrs);

  return ((t_intr != NULL_TREE)
	  || (t_excp != NULL_TREE)
	  || (t_reset != NULL_TREE));
}

/* Return true if FUNC is a isr function with critical attribute.  */
bool
nds32_isr_function_critical_p (tree func)
{
  tree t_intr;
  tree t_excp;
  tree t_critical;

  tree attrs;

  if (TREE_CODE (func) != FUNCTION_DECL)
    abort ();

  attrs = DECL_ATTRIBUTES (func);

  t_intr  = lookup_attribute ("interrupt", attrs);
  t_excp  = lookup_attribute ("exception", attrs);

  t_critical = lookup_attribute ("critical", attrs);

  /* If both interrupt and exception attribute does not appear,
     we can return false immediately.  */
  if ((t_intr == NULL_TREE) && (t_excp == NULL_TREE))
    return false;

  /* Here we can guarantee either interrupt or ecxception attribute
     does exist, so further check critical attribute.
     If it also appears, we can return true.  */
  if (t_critical != NULL_TREE)
    return true;

  /* ------------------------------------------------------------- */
  /* FIXME:
     FOR BACKWARD COMPATIBILITY, we need to handle string type.
     If the string 'critical' appears in the interrupt/exception
     string argument, we can return true.  */
  if (t_intr != NULL_TREE || t_excp != NULL_TREE)
    {
      char target_str[100];
      char *critical_str;
      tree t_check;
      tree string_arg;

      t_check = t_intr ? t_intr : t_excp;
      if (TREE_CODE (TREE_VALUE (TREE_VALUE (t_check))) == STRING_CST)
	{
	  string_arg = TREE_VALUE (TREE_VALUE (t_check));
	  strcpy (target_str, TREE_STRING_POINTER (string_arg));
	  critical_str = strstr (target_str, "critical");

	  /* Found 'critical' string, so return true.  */
	  if (critical_str)
	    return true;
	}
    }
  /* ------------------------------------------------------------- */

  /* Other cases, this isr function is not critical type.  */
  return false;
}

/* ------------------------------------------------------------- */
