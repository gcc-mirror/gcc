/* Exporting Algol 68 module interfaces.
   Copyright (C) 2025 Jose E. Marchesi.
   Copyright (C) 2010-2025 Free Software Foundation, Inc.

   Written by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "memmodel.h"
#include "tree.h"
#include "target.h"
#include "tm_p.h"
#include "simple-object.h"
#include "varasm.h"
#include "intl.h"
#include "output.h" /* for assemble_string */
#include "common/common-target.h"
#include "dwarf2asm.h"

#include <algorithm>

#include "a68.h"

#ifndef TARGET_AIX_OS
#define TARGET_AIX_OS 0
#endif

/* The size of the target's pointer type.  */
#ifndef PTR_SIZE
#define PTR_SIZE (POINTER_SIZE / BITS_PER_UNIT)
#endif

/* Create a new module interface, initially with no modes and no
   extracts. MODULE_NAME is the name of the module as it is accessed at the
   source level, which corresponds to a bold word.  */

MOIF_T *
a68_moif_new (const char *module_name)
{
  MOIF_T *moif = ggc_cleared_alloc<MOIF_T> ();

  VERSION (moif) = GA68_EXPORTS_VERSION;
  NAME (moif) = (module_name == NULL ? NULL : ggc_strdup (module_name));
  PRELUDE (moif) = NULL;
  POSTLUDE (moif) = NULL;
  NEXT (moif) = NO_MOIF;
  vec_alloc (MODES (moif), 16);
  vec_alloc (MODULES (moif), 16);
  vec_alloc (IDENTIFIERS (moif), 16);
  vec_alloc (INDICANTS (moif), 16);
  vec_alloc (PRIOS (moif), 16);
  vec_alloc (OPERATORS (moif), 16);
  return moif;
}

/* Add a new mode to a module interface.  */

static void
a68_add_moid_to_moif (MOIF_T *moif, MOID_T *m)
{
  if (! MODES(moif)->contains (m))
    vec_safe_push (MODES (moif), m);
}

/* Add a new identifier extract to a module interface.  */

void
a68_add_identifier_to_moif (MOIF_T *moif, TAG_T *tag)
{
  EXTRACT_T *e = ggc_alloc<EXTRACT_T> ();
  const char *tag_symbol = IDENTIFIER_POINTER (DECL_NAME (TAX_TREE_DECL (tag)));

  EXTRACT_KIND (e) = GA68_EXTRACT_IDEN;
  EXTRACT_SYMBOL (e) = ggc_strdup (tag_symbol);
  EXTRACT_MODE (e) = MOID (tag);
  EXTRACT_PRIO (e) = 0;
  EXTRACT_VARIABLE (e) = VARIABLE (tag);
  EXTRACT_IN_PROC (e) = IN_PROC (tag);

  if (! IDENTIFIERS (moif)->contains (e))
    {
      a68_add_moid_to_moif (moif, MOID (tag));
      vec_safe_push (IDENTIFIERS (moif), e);
    }
}

/* Add a new mode indicant extract to a module interface.  */

static void
a68_add_indicant_to_moif (MOIF_T *moif, TAG_T *tag)
{
  EXTRACT_T *e = ggc_alloc<EXTRACT_T> ();
  /* Mode tags are not associated with declarations, so we have to do the
     mangling here.  */
  tree id = a68_get_mangled_indicant (NSYMBOL (NODE (tag)), NAME (moif));
  const char *tag_symbol = IDENTIFIER_POINTER (id);

  EXTRACT_KIND (e) = GA68_EXTRACT_MODE;
  EXTRACT_SYMBOL (e) = ggc_strdup (tag_symbol);
  EXTRACT_MODE (e) = MOID (tag);
  EXTRACT_PRIO (e) = 0;
  EXTRACT_VARIABLE (e) = false;
  EXTRACT_IN_PROC (e) = false;

  if (! INDICANTS (moif)->contains (e))
    {
      a68_add_moid_to_moif (moif, MOID (tag));
      vec_safe_push (INDICANTS (moif), e);
    }
}

/* Add a new module extract to a module interface.  */

static void
a68_add_module_to_moif (MOIF_T *moif, TAG_T *tag)
{
  EXTRACT_T *e = ggc_alloc<EXTRACT_T> ();
  const char *tag_symbol = NSYMBOL (NODE (tag));

  EXTRACT_KIND (e) = GA68_EXTRACT_MODU;
  EXTRACT_SYMBOL (e) = ggc_strdup (tag_symbol);
  EXTRACT_MODE (e) = NO_MOID;
  EXTRACT_PRIO (e) = 0;
  EXTRACT_VARIABLE (e) = false;
  EXTRACT_IN_PROC (e) = false;

  if (! MODULES (moif)->contains (e))
    vec_safe_push (MODULES (moif), e);
}

/* Add a new priority extract to a module interface.  */

static void
a68_add_prio_to_moif (MOIF_T *moif, TAG_T *tag)
{
  EXTRACT_T *e = ggc_alloc<EXTRACT_T> ();
  /* Priority tags are not associated with declarations, so we have to do the
     mangling here.  */
  tree id = a68_get_mangled_indicant (NSYMBOL (NODE (tag)), NAME (moif));
  const char *tag_symbol = IDENTIFIER_POINTER (id);

  EXTRACT_KIND (e) = GA68_EXTRACT_PRIO;
  EXTRACT_SYMBOL (e) = ggc_strdup (tag_symbol);
  EXTRACT_MODE (e) = NO_MOID;
  EXTRACT_PRIO (e) = PRIO (tag);
  EXTRACT_VARIABLE (e) = false;
  EXTRACT_IN_PROC (e) = false;

  if (! PRIOS (moif)->contains (e))
    vec_safe_push (PRIOS (moif), e);
}

/* Add a new operator extract to a module interface.  */

static void
a68_add_operator_to_moif (MOIF_T *moif, TAG_T *tag)
{
  EXTRACT_T *e = ggc_alloc<EXTRACT_T> ();
  const char *tag_symbol = IDENTIFIER_POINTER (DECL_NAME (TAX_TREE_DECL (tag)));

  EXTRACT_KIND (e) = GA68_EXTRACT_OPER;
  EXTRACT_SYMBOL (e) = ggc_strdup (tag_symbol);
  EXTRACT_MODE (e) = MOID (tag);
  EXTRACT_PRIO (e) = 0;
  EXTRACT_VARIABLE (e) = EXTRACT_VARIABLE (tag);
  /* There are no operatorvariable-declarations */
  gcc_assert (EXTRACT_VARIABLE (e) == false);
  EXTRACT_IN_PROC (e) = IN_PROC (tag);

  if (! OPERATORS (moif)->contains (e))
    {
      a68_add_moid_to_moif (moif, MOID (tag));
      vec_safe_push (OPERATORS (moif), e);
    }
}

/* Make the exports section the asm_out_file's new current section.  */

static void
a68_switch_to_export_section (void)
{
  static section *exports_sec;

  if (exports_sec == NULL)
    {
      gcc_assert (targetm_common.have_named_sections);
#ifdef OBJECT_FORMAT_MACHO
      exports_sec
	= get_section (A68_EXPORT_SEGMENT_NAME "," A68_EXPORT_SECTION_NAME,
		       SECTION_DEBUG, NULL);
#else
      exports_sec = get_section (A68_EXPORT_SECTION_NAME,
				 TARGET_AIX_OS ? SECTION_EXCLUDE : SECTION_DEBUG,
				 NULL);
#endif
    }

  switch_to_section (exports_sec);
}

/* Output a sized string.  */

static void
a68_asm_output_string (const char *s, const char *comment)
{
  dw2_asm_output_data (2, strlen (s) + 1, comment);
  assemble_string (s, strlen (s) + 1);
}

/* Output a mode to the exports section if it hasn't been emitted already.  */

static void
a68_asm_output_mode (MOID_T *m, const char *module_label)
{
  /* Do nothing if the mode has been already emitted and therefore there is
     already a label to access it.  */
  if (ASM_LABEL (m) != NULL)
    return;

  /* Mode indicants are not emitted in the mode table, but as mode extracts in
     the extracts table.  Still we have to emit the named mode.  */
  if (IS (m, INDICANT))
    m = MOID (NODE (m));

  /* Collection of modes.  */
  if (IS (m, SERIES_MODE) || IS (m, STOWED_MODE))
    {
      for (PACK_T *p = PACK (m); p != NO_PACK; FORWARD (p))
	a68_asm_output_mode (MOID (p), module_label);
      return;
    }

  /* Ok we got a mode to output.  */

  /* First emit referred modes and sub-modes.  Note how we have to create a
     label for the mode and install it in the NODE_T in order to avoid infinite
     recursion in case of ref-induced recursive mode definitions.  */

  static long int cnt;
  static char label[100];
  ASM_GENERATE_INTERNAL_LABEL (label, "LMD", cnt++);
  ASM_LABEL (m) = ggc_strdup (label);

  if (IS_REF(m) || IS_FLEX (m))
    a68_asm_output_mode (SUB (m), module_label);
  else if (m != M_STRING && IS_FLEXETY_ROW (m))
    a68_asm_output_mode (SUB (m), module_label);
  else if (!IS_COMPLEX (m) && (IS_STRUCT (m) || IS_UNION (m)))
    {
      for (PACK_T *p = PACK (m); p != NO_PACK; FORWARD (p))
	a68_asm_output_mode (MOID (p), module_label);
    }
  else if (IS (m, PROC_SYMBOL))
    {
      a68_asm_output_mode (SUB (m), module_label);
      for (PACK_T *p = PACK (m); p != NO_PACK; FORWARD (p))
	a68_asm_output_mode (MOID (p), module_label);
    }

  /* No recursion below this point pls.  */

  /* Emit a label for this mode.  */
  ASM_OUTPUT_LABEL (asm_out_file, ASM_LABEL (m));

  /* Now emit assembly for the mode entry.  */
  if (m == M_VOID)
    dw2_asm_output_data (1, GA68_MODE_VOID, "void");
  else if (m == M_CHAR)
    dw2_asm_output_data (1, GA68_MODE_CHAR, "char");
  else if (m == M_BOOL)
    dw2_asm_output_data (1, GA68_MODE_BOOL, "bool");
  else if (m == M_STRING)
    dw2_asm_output_data (1, GA68_MODE_STRING, "string");
  else if (IS_INTEGRAL (m))
    {
      dw2_asm_output_data (1, GA68_MODE_INT, "int");
      dw2_asm_output_data (1, DIM (m), "sizety");
    }
  else if (IS_REAL (m))
    {
      dw2_asm_output_data (1, GA68_MODE_REAL, "real");
      dw2_asm_output_data (1, DIM (m), "sizety");
    }
  else if (IS_BITS (m))
    {
      dw2_asm_output_data (1, GA68_MODE_BITS, "bits");
      dw2_asm_output_data (1, DIM (m), "sizety");
    }
  else if (IS_BYTES (m))
    {
      dw2_asm_output_data (1, GA68_MODE_BYTES, "bytes");
      dw2_asm_output_data (1, DIM (m), "sizety");
    }
  else if (IS_COMPLEX (m))
    {
      /* Complex is a struct of two reals of the right sizety.  */
      int dim = DIM (MOID (PACK (m)));
      dw2_asm_output_data (1, GA68_MODE_CMPL, "compl");
      dw2_asm_output_data (1, dim, "sizety");
    }
  else if (IS_REF (m))
    {
      dw2_asm_output_data (1, GA68_MODE_NAME, "ref");
      dw2_asm_output_delta (PTR_SIZE, ASM_LABEL (SUB (m)), module_label, "referred mode");
    }
  else if (IS_FLEX (m))
    {
      dw2_asm_output_data (1, GA68_MODE_FLEX, "flex");
      dw2_asm_output_delta (PTR_SIZE, ASM_LABEL (SUB (m)), module_label, "flexible row mode");
    }
  else if (IS_ROW (m))
    {
      dw2_asm_output_data (1, GA68_MODE_ROW, "row");
      dw2_asm_output_data (1, DIM (m), "dim");
      /* XXX for now emit zeroes as triplets.  */
      for (int i = 0; i < DIM (m); ++i)
	{
	  dw2_asm_output_data (PTR_SIZE, 0, "lb");
	  dw2_asm_output_data (PTR_SIZE, 0, "ub");
	}
      dw2_asm_output_delta (PTR_SIZE, ASM_LABEL (SUB (m)), module_label, "row of");
    }
  else if (IS_STRUCT (m))
    {
      dw2_asm_output_data (1, GA68_MODE_STRUCT, "struct");
      dw2_asm_output_data (2, DIM (m), "nfields");
      for (PACK_T *p = PACK (m); p != NO_PACK; FORWARD (p))
	{
	  dw2_asm_output_delta (PTR_SIZE, ASM_LABEL (MOID (p)), module_label, "field mode");
	  if (TEXT (p) != NO_TEXT)
	    a68_asm_output_string (TEXT (p), "field name");
	  else
	    a68_asm_output_string ("", "field name");
	}
    }
  else if (IS_UNION (m))
    {
      dw2_asm_output_data (1, GA68_MODE_UNION, "union");
      dw2_asm_output_data (2, DIM (m), "nmodes");
      for (PACK_T *p = PACK (m); p != NO_PACK; FORWARD (p))
	dw2_asm_output_delta (PTR_SIZE, ASM_LABEL (MOID (p)), module_label, "united mode");
    }
  else if (IS (m, PROC_SYMBOL))
    {
      dw2_asm_output_data (1, GA68_MODE_PROC, "proc");
      dw2_asm_output_delta (PTR_SIZE, ASM_LABEL (SUB (m)), module_label, "ret mode");
      dw2_asm_output_data (1, DIM (m), "nargs");
      for (PACK_T *p = PACK (m); p != NO_PACK; FORWARD (p))
	{
	  dw2_asm_output_delta (PTR_SIZE, ASM_LABEL (MOID (p)), module_label, "arg mode");
	  if (TEXT (p) != NO_TEXT)
	    a68_asm_output_string (TEXT (p), "arg name");
	  else
	    a68_asm_output_string ("", "arg name");
	}
    }
  else
    dw2_asm_output_data (1, GA68_MODE_UNKNOWN, "unknown mode %s",
			 a68_moid_to_string (m, 80, NO_NODE, false));
}

/* Output an extract for a given tag to the extracts section.  */

static void
a68_asm_output_extract (const char *module_label, int kind,
			const char *symbol, MOID_T *mode, int prio,
			bool variable, bool in_proc)
{
  static char begin_label[100];
  static char end_label[100];
  static long int cnt;

  ASM_GENERATE_INTERNAL_LABEL (begin_label, "LEBL", cnt);
  ASM_GENERATE_INTERNAL_LABEL (end_label, "LEEL", cnt);
  cnt++;

  dw2_asm_output_delta (PTR_SIZE, end_label, begin_label, "extract size");
  ASM_OUTPUT_LABEL (asm_out_file, begin_label);

  bool encode_mdextra = false;
  switch (kind)
    {
    case GA68_EXTRACT_MODU:
      dw2_asm_output_data (1, GA68_EXTRACT_MODU, "module extract %s", symbol);
      a68_asm_output_string (symbol, "module indication");
      break;
    case GA68_EXTRACT_MODE:
      dw2_asm_output_data (1, GA68_EXTRACT_MODE, "mode extract %s", symbol);
      a68_asm_output_string (symbol, "mode indication");
      dw2_asm_output_delta (PTR_SIZE, ASM_LABEL (mode), module_label, "mode");
      break;
    case GA68_EXTRACT_IDEN:
      dw2_asm_output_data (1, GA68_EXTRACT_IDEN, "identifier extract %s", symbol);
      a68_asm_output_string (symbol, "name");
      dw2_asm_output_delta (PTR_SIZE, ASM_LABEL (mode), module_label, "mode");
      encode_mdextra = true;
      break;
    case GA68_EXTRACT_PRIO:
      dw2_asm_output_data (1, GA68_EXTRACT_PRIO, "prio extract %s", symbol);
      a68_asm_output_string (symbol, "opname");
      dw2_asm_output_data (1, prio, "priority");
      break;
    case GA68_EXTRACT_OPER:
      dw2_asm_output_data (1, GA68_EXTRACT_OPER, "operator extract %s", symbol);
      a68_asm_output_string (symbol, "opname");
      dw2_asm_output_delta (PTR_SIZE, ASM_LABEL (mode), module_label, "mode");
      encode_mdextra = true;
      break;
    default:
      gcc_unreachable ();
    }

  if (encode_mdextra)
    {
      dw2_asm_output_data (PTR_SIZE, 2, "mdextra size");
      dw2_asm_output_data (1, variable, "variable");
      dw2_asm_output_data (1, in_proc, "in_proc");
    }
  else
    dw2_asm_output_data (PTR_SIZE, 0, "mdextra size");

  ASM_OUTPUT_LABEL (asm_out_file, end_label);
}

/* Output a module interface.  */

static void
a68_asm_output_moif (MOIF_T *moif)
{
  a68_switch_to_export_section ();

  static char module_label[100];
  static long int moifcnt;
  ASM_GENERATE_INTERNAL_LABEL (module_label, "LMOIF", moifcnt++);
  ASM_OUTPUT_LABEL (asm_out_file, module_label);

  if (flag_debug_asm)
    {
      fputs (ASM_COMMENT_START " MODIF START ", asm_out_file);
      fputs (NAME (moif), asm_out_file);
      fputc ('\n', asm_out_file);
    }

  dw2_asm_output_data (1, A68_EXPORT_MAGIC1, "magic1");
  dw2_asm_output_data (1, A68_EXPORT_MAGIC2, "magic2");
  dw2_asm_output_data (2, VERSION (moif), "exports version");
  a68_asm_output_string (NAME (moif), "module name");
  a68_asm_output_string (PRELUDE (moif) ? PRELUDE (moif) : "", "prelude symbol");
  a68_asm_output_string (POSTLUDE (moif) ? POSTLUDE (moif) : "", "postlude symbol");

  /* Modes table.  */
  static char modes_begin_label[100];
  static char modes_end_label[100];
  static long int modescnt;
  ASM_GENERATE_INTERNAL_LABEL (modes_begin_label, "LMTL", modescnt++);
  ASM_GENERATE_INTERNAL_LABEL (modes_end_label, "LMTL", modescnt++);

  if (flag_debug_asm)
    fputs ("\t" ASM_COMMENT_START " modes table\n", asm_out_file);
  dw2_asm_output_delta (PTR_SIZE, modes_end_label, modes_begin_label,
			"modes size");
  ASM_OUTPUT_LABEL (asm_out_file, modes_begin_label);
  for (MOID_T *m : MODES (moif))
    a68_asm_output_mode (m, module_label);
  ASM_OUTPUT_LABEL (asm_out_file, modes_end_label);

  /* Extracts table.  */
  static char extracts_begin_label[100];
  static char extracts_end_label[100];
  static long int extractscnt;
  ASM_GENERATE_INTERNAL_LABEL (extracts_begin_label, "LETL", extractscnt++);
  ASM_GENERATE_INTERNAL_LABEL (extracts_end_label, "LETL", extractscnt++);

  if (flag_debug_asm)
    fputs ("\t" ASM_COMMENT_START " extracts table\n", asm_out_file);
  dw2_asm_output_delta (PTR_SIZE, extracts_end_label, extracts_begin_label,
			"extracts size");
  ASM_OUTPUT_LABEL (asm_out_file, extracts_begin_label);
  for (EXTRACT_T *e : MODULES (moif))
    a68_asm_output_extract (module_label, GA68_EXTRACT_MODU,
			    EXTRACT_SYMBOL (e), EXTRACT_MODE (e), EXTRACT_PRIO (e),
			    EXTRACT_VARIABLE (e), EXTRACT_IN_PROC (e));
  for (EXTRACT_T *e : INDICANTS (moif))
    a68_asm_output_extract (module_label, GA68_EXTRACT_MODE,
			    EXTRACT_SYMBOL (e), EXTRACT_MODE (e), EXTRACT_PRIO (e),
			    EXTRACT_VARIABLE (e), EXTRACT_IN_PROC (e));
  for (EXTRACT_T *e : IDENTIFIERS (moif))
    a68_asm_output_extract (module_label, GA68_EXTRACT_IDEN,
			    EXTRACT_SYMBOL (e), EXTRACT_MODE (e), EXTRACT_PRIO (e),
			    EXTRACT_VARIABLE (e), EXTRACT_IN_PROC (e));
  for (EXTRACT_T *e : PRIOS (moif))
    a68_asm_output_extract (module_label, GA68_EXTRACT_PRIO,
			    EXTRACT_SYMBOL (e), EXTRACT_MODE (e), EXTRACT_PRIO (e),
			    EXTRACT_VARIABLE (e), EXTRACT_IN_PROC (e));
  for (EXTRACT_T *e : OPERATORS (moif))
    a68_asm_output_extract (module_label, GA68_EXTRACT_OPER,
			    EXTRACT_SYMBOL (e), EXTRACT_MODE (e), EXTRACT_PRIO (e),
			    EXTRACT_VARIABLE (e), EXTRACT_IN_PROC (e));
  ASM_OUTPUT_LABEL (asm_out_file, extracts_end_label);

  if (flag_debug_asm)
    {
      fputs (ASM_COMMENT_START " MODIF END ", asm_out_file);
      fputs (NAME (moif), asm_out_file);
      fputc ('\n', asm_out_file);
    }
}

/* Add module exports for publicized module revelations.  */

static void
add_pub_revelations_to_moif (MOIF_T *moif, NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, PUBLIC_SYMBOL))
	{
	  gcc_assert (IS (NEXT (p), MODULE_INDICANT));
	  TAG_T *tag = a68_new_tag ();
	  NODE (tag) = NEXT (p);
	  a68_add_module_to_moif (moif, tag);
	  FORWARD (p);
	}
      else
	add_pub_revelations_to_moif (moif, SUB (p));
    }
}

/* Emit export information for the module definition in the parse tree P.  */

void
a68_do_exports (NODE_T *p)
{
  for (;p != NO_NODE; FORWARD (p))
    {
      if (IS (p, DEFINING_MODULE_INDICANT))
	{
	  tree module_id = a68_get_mangled_indicant (NSYMBOL (p));
	  MOIF_T *moif = a68_moif_new (IDENTIFIER_POINTER (module_id));
	  char *prelude = xasprintf ("%s__prelude", IDENTIFIER_POINTER (module_id));
	  char *postlude = xasprintf ("%s__postlude", IDENTIFIER_POINTER (module_id));
	  PRELUDE (moif) = ggc_strdup (prelude);
	  POSTLUDE (moif) = ggc_strdup (postlude);
	  free (prelude);
	  free (postlude);

	  NODE_T *module_text = NEXT (NEXT (p));
	  gcc_assert (IS (module_text, MODULE_TEXT));

	  /* Get modules exports from the revelation part.  */
	  if (IS (SUB (module_text), REVELATION_PART))
	    {
	      NODE_T *revelation_part = SUB (module_text);
	      add_pub_revelations_to_moif (moif, SUB (revelation_part));
	    }

	  NODE_T *def_part = (IS (SUB (module_text), REVELATION_PART)
			      ? NEXT_SUB (module_text)
			      : SUB (module_text));
	  gcc_assert (IS (def_part, DEF_PART));
	  TABLE_T *table = TABLE (SUB (def_part));
	  gcc_assert (PUBLIC_RANGE (table));

	  for (TAG_T *t = INDICANTS (table); t != NO_TAG; FORWARD (t))
	    {
	      if (PUBLICIZED (t))
		a68_add_indicant_to_moif (moif, t);
	    }

	  for (TAG_T *t = IDENTIFIERS (table); t != NO_TAG; FORWARD (t))
	    {
	      if (PUBLICIZED (t))
		a68_add_identifier_to_moif (moif, t);
	    }

	  for (TAG_T *t = PRIO (table); t != NO_TAG; FORWARD (t))
	    {
	      if (PUBLICIZED (t))
		a68_add_prio_to_moif (moif, t);
	    }

	  for (TAG_T *t = OPERATORS (table); t != NO_TAG; FORWARD (t))
	    {
	      if (PUBLICIZED (t))
		a68_add_operator_to_moif (moif, t);
	    }

	  a68_asm_output_moif (moif);
	  if (flag_a68_dump_moif)
	    a68_dump_moif (moif);
	}
      else
	a68_do_exports (SUB (p));
    }
}
