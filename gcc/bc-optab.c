/* Bytecode conversion definitions for GNU C-compiler.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include "tree.h"
#include "rtl.h"
#include "machmode.h"
#include "obstack.h"
#include "bytecode.h"
#include "bc-typecd.h"
#include "bc-opcode.h"
#include "bc-optab.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern char *xmalloc ();
extern void free ();

/* Table relating interpreter typecodes to machine modes.  */
#define GET_TYPECODE_MODE(CODE) (typecode_mode[((int) CODE)])
enum machine_mode typecode_mode[] = {
#define DEFTYPECODE(CODE, NAME, MODE, TYPE) MODE,
#include "bc-typecd.def"
#undef DEFTYPECODE
};

/* Machine mode to type code map */
static enum typecode signed_mode_to_code_map[MAX_MACHINE_MODE+1];
static enum typecode unsigned_mode_to_code_map[MAX_MACHINE_MODE+1];

#define GET_TYPECODE_SIZE(CODE) GET_MODE_SIZE (GET_TYPECODE_MODE (CODE))

#define BIG_ARBITRARY_NUMBER 100000

/* Table of recipes for conversions among scalar types, to be filled
   in as needed at run time.  */
static struct conversion_recipe
{
  unsigned char *opcodes;	/* Bytecodes to emit in order.  */
  int nopcodes;			/* Count of bytecodes. */
  int cost;			/* A rather arbitrary cost function. */
} conversion_recipe[NUM_TYPECODES][NUM_TYPECODES];

/* Binary operator tables.  */
struct binary_operator optab_plus_expr[] = {
  { addSI, SIcode, SIcode, SIcode },
  { addDI, DIcode, DIcode, DIcode },
  { addSF, SFcode, SFcode, SFcode },
  { addDF, DFcode, DFcode, DFcode },
  { addXF, XFcode, XFcode, XFcode },
  { addPSI, Pcode, Pcode, SIcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_minus_expr[] = {
  { subSI, SIcode, SIcode, SIcode },
  { subDI, DIcode, DIcode, DIcode },
  { subSF, SFcode, SFcode, SFcode },
  { subDF, DFcode, DFcode, DFcode },
  { subXF, XFcode, XFcode, XFcode },
  { subPP, SIcode, Pcode, Pcode },
  { -1, -1, -1, -1 },
};

/* The ordering of the tables for multiplicative operators
   is such that unsigned operations will be preferred to signed
   operations when one argument is unsigned.  */

struct binary_operator optab_mult_expr[] = {
  { mulSU, SUcode, SUcode, SUcode },
  { mulDU, DUcode, DUcode, DUcode },
  { mulSI, SIcode, SIcode, SIcode },
  { mulDI, DIcode, DIcode, DIcode },
  { mulSF, SFcode, SFcode, SFcode },
  { mulDF, DFcode, DFcode, DFcode },
  { mulXF, XFcode, XFcode, XFcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_trunc_div_expr[] = {
  { divSU, SUcode, SUcode, SUcode },
  { divDU, DUcode, DUcode, DUcode },
  { divSI, SIcode, SIcode, SIcode },
  { divDI, DIcode, DIcode, DIcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_trunc_mod_expr[] = {
  { modSU, SUcode, SUcode, SUcode },
  { modDU, DUcode, DUcode, DUcode },
  { modSI, SIcode, SIcode, SIcode },
  { modDI, DIcode, DIcode, DIcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_rdiv_expr[] = {
  { divSF, SFcode, SFcode, SFcode },
  { divDF, DFcode, DFcode, DFcode },
  { divXF, XFcode, XFcode, XFcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_bit_and_expr[] = {
  { andSI, SIcode, SIcode, SIcode },
  { andDI, DIcode, DIcode, DIcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_bit_ior_expr[] = {
  { iorSI, SIcode, SIcode, SIcode },
  { iorDI, DIcode, DIcode, DIcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_bit_xor_expr[] = {
  { xorSI, SIcode, SIcode, SIcode },
  { xorDI, DIcode, DIcode, DIcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_lshift_expr[] = {
  { lshiftSI, SIcode, SIcode, SIcode },
  { lshiftSU, SUcode, SUcode, SIcode },
  { lshiftDI, DIcode, DIcode, SIcode },
  { lshiftDU, DUcode, DUcode, SIcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_rshift_expr[] = {
  { rshiftSI, SIcode, SIcode, SIcode },
  { rshiftSU, SUcode, SUcode, SIcode },
  { rshiftDI, DIcode, DIcode, SIcode },
  { rshiftDU, DUcode, DUcode, SIcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_truth_and_expr[] = {
  { andSI, SIcode, Tcode, Tcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_truth_or_expr[] = {
  { iorSI, SIcode, Tcode, Tcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_lt_expr[] = {
  { ltSI, Tcode, SIcode, SIcode },
  { ltSU, Tcode, SUcode, SUcode },
  { ltDI, Tcode, DIcode, DIcode },
  { ltDU, Tcode, DUcode, DUcode },
  { ltSF, Tcode, SFcode, SFcode },
  { ltDF, Tcode, DFcode, DFcode },
  { ltXF, Tcode, XFcode, XFcode },
  { ltP, Tcode, Pcode, Pcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_le_expr[] = {
  { leSI, Tcode, SIcode, SIcode },
  { leSU, Tcode, SUcode, SUcode },
  { leDI, Tcode, DIcode, DIcode },
  { leDU, Tcode, DUcode, DUcode },
  { leSF, Tcode, SFcode, SFcode },
  { leDF, Tcode, DFcode, DFcode },
  { leXF, Tcode, XFcode, XFcode },
  { leP, Tcode, Pcode, Pcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_ge_expr[] = {
  { geSI, Tcode, SIcode, SIcode },
  { geSU, Tcode, SUcode, SUcode },
  { geDI, Tcode, DIcode, DIcode },
  { geDU, Tcode, DUcode, DUcode },
  { geSF, Tcode, SFcode, SFcode },
  { geDF, Tcode, DFcode, DFcode },
  { geXF, Tcode, XFcode, XFcode },
  { geP, Tcode, Pcode, Pcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_gt_expr[] = {
  { gtSI, Tcode, SIcode, SIcode },
  { gtSU, Tcode, SUcode, SUcode },
  { gtDI, Tcode, DIcode, DIcode },
  { gtDU, Tcode, DUcode, DUcode },
  { gtSF, Tcode, SFcode, SFcode },
  { gtDF, Tcode, DFcode, DFcode },
  { gtXF, Tcode, XFcode, XFcode },
  { gtP, Tcode, Pcode, Pcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_eq_expr[] = {
  { eqSI, Tcode, SIcode, SIcode },
  { eqDI, Tcode, DIcode, DIcode },
  { eqSF, Tcode, SFcode, SFcode },
  { eqDF, Tcode, DFcode, DFcode },
  { eqXF, Tcode, XFcode, XFcode },
  { eqP, Tcode, Pcode, Pcode },
  { -1, -1, -1, -1 },
};

struct binary_operator optab_ne_expr[] = {
  { neSI, Tcode, SIcode, SIcode },
  { neDI, Tcode, DIcode, DIcode },
  { neSF, Tcode, SFcode, SFcode },
  { neDF, Tcode, DFcode, DFcode },
  { neXF, Tcode, XFcode, XFcode },
  { neP, Tcode, Pcode, Pcode },
  { -1, -1, -1, -1 },
};

/* Unary operator tables.  */
struct unary_operator optab_negate_expr[] = {
  { negSI, SIcode, SIcode },
  { negDI, DIcode, DIcode },
  { negSF, SFcode, SFcode },
  { negDF, DFcode, DFcode },
  { negXF, XFcode, XFcode },
  { -1, -1, -1 },
};

struct unary_operator optab_bit_not_expr[] = {
  { notSI, SIcode, SIcode },
  { notDI, DIcode, DIcode },
  { -1, -1, -1 },
};

struct unary_operator optab_truth_not_expr[] = {
  { notT, SIcode, SIcode },
  { -1, -1, -1 },
};

/* Increment operator tables.  */
struct increment_operator optab_predecrement_expr[] = {
  { predecQI, QIcode },
  { predecQI, QUcode },
  { predecHI, HIcode },
  { predecHI, HUcode },
  { predecSI, SIcode },
  { predecSI, SUcode },
  { predecDI, DIcode },
  { predecDI, DUcode },
  { predecP, Pcode },
  { predecSF, SFcode },
  { predecDF, DFcode },
  { predecXF, XFcode },
  { -1, -1 },
};

struct increment_operator optab_preincrement_expr[] = {
  { preincQI, QIcode },
  { preincQI, QUcode },
  { preincHI, HIcode },
  { preincHI, HUcode },
  { preincSI, SIcode },
  { preincSI, SUcode },
  { preincDI, DIcode },
  { preincDI, DUcode },
  { preincP, Pcode },
  { preincSF, SFcode },
  { preincDF, DFcode },
  { preincXF, XFcode },
  { -1, -1 },
};

struct increment_operator optab_postdecrement_expr[] = {
  { postdecQI, QIcode },
  { postdecQI, QUcode },
  { postdecHI, HIcode },
  { postdecHI, HUcode },
  { postdecSI, SIcode },
  { postdecSI, SUcode },
  { postdecDI, DIcode },
  { postdecDI, DUcode },
  { postdecP, Pcode },
  { postdecSF, SFcode },
  { postdecDF, DFcode },
  { postdecXF, XFcode },
  { -1, -1 },
};

struct increment_operator optab_postincrement_expr[] = {
  { postincQI, QIcode },
  { postincQI, QUcode },
  { postincHI, HIcode },
  { postincHI, HUcode },
  { postincSI, SIcode },
  { postincSI, SUcode },
  { postincDI, DIcode },
  { postincDI, DUcode },
  { postincP, Pcode },
  { postincSF, SFcode },
  { postincDF, DFcode },
  { postincXF, XFcode },
  { -1, -1 },
};

/* Table of conversions supported by the interpreter.  */
static struct conversion_info
{
  enum bytecode_opcode opcode;	/*  here indicates the conversion needs no opcode.  */
  enum typecode from;
  enum typecode to;
  int cost;			/* 1 for no-op conversions, 2 for widening conversions,
				   4 for int/float conversions, 8 for narrowing conversions.  */
} conversion_info[] = {
  { -1, QIcode, QUcode, 1 },
  { -1, HIcode, HUcode, 1 },
  { -1, SIcode, SUcode, 1 },
  { -1, DIcode, DUcode, 1 },
  { -1, QUcode, QIcode, 1 },
  { -1, HUcode, HIcode, 1 },
  { -1, SUcode, SIcode, 1 },
  { -1, DUcode, DIcode, 1 },
  { -1, Tcode, SIcode, 1 },
  { convertQIHI, QIcode, HIcode, 2 },
  { convertQUHU, QUcode, HUcode, 2 },
  { convertQUSU, QUcode, SUcode, 2 },
  { convertHISI, HIcode, SIcode, 2 },
  { convertHUSU, HUcode, SUcode, 2 },
  { convertSIDI, SIcode, DIcode, 2 },
  { convertSUDU, SUcode, DUcode, 2 },
  { convertSFDF, SFcode, DFcode, 2 },
  { convertDFXF, DFcode, XFcode, 2 },
  { convertHIQI, HIcode, QIcode, 8 },
  { convertSIQI, SIcode, QIcode, 8 },
  { convertSIHI, SIcode, HIcode, 8 },
  { convertSUQU, SUcode, QUcode, 8 },
  { convertDISI, DIcode, SIcode, 8 },
  { convertDFSF, DFcode, SFcode, 8 },
  { convertXFDF, XFcode, DFcode, 8 },
  { convertPSI, Pcode, SIcode, 2 },
  { convertSIP, SIcode, Pcode, 2 },
  { convertSIT, SIcode, Tcode, 2 },
  { convertDIT, DIcode, Tcode, 2 },
  { convertSFT, SFcode, Tcode, 2 },
  { convertDFT, DFcode, Tcode, 2 },
  { convertXFT, XFcode, Tcode, 2 },
  { convertQISI, QIcode, SIcode, 2 },
  { convertPT, Pcode, Tcode, 2 },
  { convertSISF, SIcode, SFcode, 4 },
  { convertSIDF, SIcode, DFcode, 4 },
  { convertSIXF, SIcode, XFcode, 4 },
  { convertSUSF, SUcode, SFcode, 4 },
  { convertSUDF, SUcode, DFcode, 4 },
  { convertSUXF, SUcode, XFcode, 4 },
  { convertDISF, DIcode, SFcode, 4 },
  { convertDIDF, DIcode, DFcode, 4 },
  { convertDIXF, DIcode, XFcode, 4 },
  { convertDUSF, DUcode, SFcode, 4 },
  { convertDUDF, DUcode, DFcode, 4 },
  { convertDUXF, DUcode, XFcode, 4 },
  { convertSFSI, SFcode, SIcode, 4 },
  { convertDFSI, DFcode, SIcode, 4 },
  { convertXFSI, XFcode, SIcode, 4 },
  { convertSFSU, SFcode, SUcode, 4 },
  { convertDFSU, DFcode, SUcode, 4 },
  { convertXFSU, XFcode, SUcode, 4 },
  { convertSFDI, SFcode, DIcode, 4 },
  { convertDFDI, DFcode, DIcode, 4 },
  { convertXFDI, XFcode, DIcode, 4 },
  { convertSFDU, SFcode, DUcode, 4 },
  { convertDFDU, DFcode, DUcode, 4 },
  { convertXFDU, XFcode, DUcode, 4 },
  { convertSIQI, SIcode, QIcode, 8 },
};

#define NUM_CONVERSIONS (sizeof conversion_info / sizeof (struct conversion_info))

/* List form of a conversion recipe.  */
struct conversion_list
{
  enum bytecode_opcode opcode;
  enum typecode to;
  int cost;
  struct conversion_list *prev;
};

/* Determine if it is "reasonable" to add a given conversion to
   a given list of conversions.  The following criteria define
   "reasonable" conversion lists:
   * No typecode appears more than once in the sequence (no loops).
   * At most one conversion from integer to float or vice versa is present.
   * Either sign extensions or zero extensions may be present, but not both.
   * No widening conversions occur after a signed/unsigned conversion.
   * The sequence of sizes must be strict nonincreasing or nondecreasing.  */
static int
conversion_reasonable_p (conversion, list)
     struct conversion_info *conversion;
     struct conversion_list *list;
{
  struct conversion_list *curr;
  int curr_size, prev_size;
  int has_int_float, has_float_int;
  int has_sign_extend, has_zero_extend;
  int has_signed_unsigned, has_unsigned_signed;

  has_int_float = 0;
  has_float_int = 0;
  has_sign_extend = 0;
  has_zero_extend = 0;
  has_signed_unsigned = 0;
  has_unsigned_signed = 0;

  /* Make sure the destination typecode doesn't already appear in
     the list.  */
  for (curr = list; curr; curr = curr->prev)
    if (conversion->to == curr->to)
      return 0;

  /* Check for certain kinds of conversions.  */
  if (TYPECODE_INTEGER_P (conversion->from)
      && TYPECODE_FLOAT_P (conversion->to))
    has_int_float = 1;
  if (TYPECODE_FLOAT_P (conversion->from)
      && TYPECODE_INTEGER_P (conversion->to))
    has_float_int = 1;
  if (TYPECODE_SIGNED_P (conversion->from)
      && TYPECODE_SIGNED_P (conversion->to)
      && GET_TYPECODE_SIZE (conversion->from)
      < GET_TYPECODE_SIZE (conversion->to))
    has_sign_extend = 1;
  if (TYPECODE_UNSIGNED_P (conversion->from)
      && TYPECODE_UNSIGNED_P (conversion->to)
      && GET_TYPECODE_SIZE (conversion->from)
      < GET_TYPECODE_SIZE (conversion->to))
    has_zero_extend = 1;

  for (curr = list; curr && curr->prev; curr = curr->prev)
    {
      if (TYPECODE_INTEGER_P (curr->prev->to)
	  && TYPECODE_FLOAT_P (curr->to))
	has_int_float = 1;
      if (TYPECODE_FLOAT_P (curr->prev->to)
	  && TYPECODE_INTEGER_P (curr->to))
	has_float_int = 1;
      if (TYPECODE_SIGNED_P (curr->prev->to)
	  && TYPECODE_SIGNED_P (curr->to)
	  && GET_TYPECODE_SIZE (curr->prev->to)
	  < GET_TYPECODE_SIZE (curr->to))
	has_sign_extend = 1;
      if (TYPECODE_UNSIGNED_P (curr->prev->to)
	  && TYPECODE_UNSIGNED_P (curr->to)
	  && GET_TYPECODE_SIZE (curr->prev->to)
	  < GET_TYPECODE_SIZE (curr->to))
	has_zero_extend = 1;
      if (TYPECODE_SIGNED_P (curr->prev->to)
	  && TYPECODE_UNSIGNED_P (curr->to))
	has_signed_unsigned = 1;
      if (TYPECODE_UNSIGNED_P (curr->prev->to)
	  && TYPECODE_SIGNED_P (curr->to))
	has_unsigned_signed = 1;
    }

  if (TYPECODE_INTEGER_P (conversion->from)
      && TYPECODE_INTEGER_P (conversion->to)
      && GET_TYPECODE_SIZE (conversion->to)
      > GET_TYPECODE_SIZE (conversion->from)
      && (has_signed_unsigned || has_unsigned_signed))
    return 0;

  if (has_float_int && has_int_float || has_sign_extend && has_zero_extend)
    return 0;

  /* Make sure the sequence of destination typecode sizes is
     strictly nondecreasing or strictly nonincreasing.  */
  prev_size = GET_TYPECODE_SIZE (conversion->to);
  for (curr = list; curr; curr = curr->prev)
    {
      curr_size = GET_TYPECODE_SIZE (curr->to);
      if (curr_size != prev_size)
	break;
    }
  if (!curr)
    return 1;

  if (curr_size < prev_size)
    for (prev_size = curr_size; curr; curr = curr->prev)
      {
	curr_size = GET_TYPECODE_SIZE (curr->to);
	if (curr_size > prev_size)
	  return 0;
	prev_size = curr_size;
      }
  else
    for (prev_size = curr_size; curr; curr = curr->prev)
      {
	curr_size = GET_TYPECODE_SIZE (curr->to);
	if (curr_size < prev_size)
	  return 0;
	prev_size = curr_size;
      }
  return 1;
}


/* Exhaustively search all reasonable conversions to find one to
   convert the given types.  */
static struct conversion_recipe
deduce_conversion (from, to)
     enum typecode from, to;
{
  struct rl
    {
      struct conversion_list *list;
      struct rl *next;
    } *prev, curr, *good, *temp;
  struct conversion_list *conv, *best;
  int i, cost, bestcost;
  struct conversion_recipe result;
  struct obstack recipe_obstack;


  obstack_init (&recipe_obstack);
  curr.next = (struct rl *) obstack_alloc (&recipe_obstack, sizeof (struct rl));
  curr.next->list =
    (struct conversion_list *) obstack_alloc (&recipe_obstack,
					      sizeof (struct conversion_list));
  curr.next->list->opcode = -1;
  curr.next->list->to = from;
  curr.next->list->cost = 0;
  curr.next->list->prev = 0;
  curr.next->next = 0;
  good = 0;

  while (curr.next)
    {
      /* Remove successful conversions from further consideration.  */
      for (prev = &curr; prev; prev = prev->next)
	if (prev->next && prev->next->list->to == to)
	  {
	    temp = prev->next->next;
	    prev->next->next = good;
	    good = prev->next;
	    prev->next = temp;
	  }

      /* Go through each of the pending conversion chains, trying
	 all possible candidate conversions on them.  */
      for (prev = curr.next, curr.next = 0; prev; prev = prev->next)
	for (i = 0; i < NUM_CONVERSIONS; ++i)
	  if (conversion_info[i].from == prev->list->to
	      && conversion_reasonable_p (&conversion_info[i], prev->list))
	    {
	      temp = (struct rl *) obstack_alloc (&recipe_obstack,
						  sizeof (struct rl));
	      temp->list = (struct conversion_list *)
		obstack_alloc (&recipe_obstack,
			       sizeof (struct conversion_list));
	      temp->list->opcode = conversion_info[i].opcode;
	      temp->list->to = conversion_info[i].to;
	      temp->list->cost = conversion_info[i].cost;
	      temp->list->prev = prev->list;
	      temp->next = curr.next;
	      curr.next = temp;
	    }
    }

  bestcost = BIG_ARBITRARY_NUMBER;
  best = 0;
  for (temp = good; temp; temp = temp->next)
    {
      for (conv = temp->list, cost = 0; conv; conv = conv->prev)
	cost += conv->cost;
      if (cost < bestcost)
	{
	  bestcost = cost;
	  best = temp->list;
	}
    }

  if (!best)
    abort ();

  for (i = 0, conv = best; conv; conv = conv->prev)
    if (conv->opcode != -1)
      ++i;

  result.opcodes = (unsigned char *) xmalloc (i);
  result.nopcodes = i;
  for (conv = best; conv; conv = conv->prev)
    if (conv->opcode != -1)
      result.opcodes[--i] = conv->opcode;
  result.cost = bestcost;
  obstack_free (&recipe_obstack, 0);
  return result;
}

#define DEDUCE_CONVERSION(FROM, TO)				\
  (conversion_recipe[(int) FROM][(int) TO].opcodes ? 0		\
   : (conversion_recipe[(int) FROM][(int) TO]			\
       = deduce_conversion (FROM, TO), 0))


/* Emit a conversion between the given scalar types.  */
void
emit_typecode_conversion (from, to)
     enum typecode from, to;
{
  int i;

  DEDUCE_CONVERSION (from, to);
  for (i = 0; i < conversion_recipe[(int) from][(int) to].nopcodes; ++i)
    bc_emit_instruction (conversion_recipe[(int) from][(int) to].opcodes[i]);
}


/* Initialize mode_to_code_map[] */
void
bc_init_mode_to_code_map ()
{
  int mode;

  for (mode = 0; mode < MAX_MACHINE_MODE + 1; mode++)
    {
      signed_mode_to_code_map[mode] = 
	unsigned_mode_to_code_map[mode] =
	  LAST_AND_UNUSED_TYPECODE;
    }

#define DEF_MODEMAP(SYM, CODE, UCODE, CONST, LOAD, STORE) \
  { signed_mode_to_code_map[(int) SYM] = CODE; \
    unsigned_mode_to_code_map[(int) SYM] = UCODE; }
#include "modemap.def"
#undef DEF_MODEMAP

  /* Initialize opcode maps for const, load, and store */
  bc_init_mode_to_opcode_maps ();
}

/* Given a machine mode return the preferred typecode.  */
enum typecode
preferred_typecode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  enum typecode code = (unsignedp
			? unsigned_mode_to_code_map
			: signed_mode_to_code_map) [MIN ((int) mode,
							 (int) MAX_MACHINE_MODE)];

  if (code == LAST_AND_UNUSED_TYPECODE)
    abort ();

  return code;
}


/* Expand a conversion between the given types.  */
void
bc_expand_conversion (from, to)
     tree from, to;
{
  enum typecode fcode, tcode;

  fcode = preferred_typecode (TYPE_MODE (from), TREE_UNSIGNED (from));
  tcode = preferred_typecode (TYPE_MODE (to), TREE_UNSIGNED (to));

  emit_typecode_conversion (fcode, tcode);
}

/* Expand a conversion of the given type to a truth value.  */
void
bc_expand_truth_conversion (from)
     tree from;
{
  enum typecode fcode;

  fcode = preferred_typecode (TYPE_MODE (from), TREE_UNSIGNED (from));
  emit_typecode_conversion (fcode, Tcode);
}

/* Emit an appropriate binary operation.  */
void
bc_expand_binary_operation (optab, resulttype, arg0, arg1)
     struct binary_operator optab[];
     tree resulttype, arg0, arg1;
{
  int i, besti, cost, bestcost;
  enum typecode resultcode, arg0code, arg1code;
  
  resultcode = preferred_typecode (TYPE_MODE (resulttype), TREE_UNSIGNED (resulttype));
  arg0code = preferred_typecode (TYPE_MODE (TREE_TYPE (arg0)), TREE_UNSIGNED (resulttype));
  arg1code = preferred_typecode (TYPE_MODE (TREE_TYPE (arg1)), TREE_UNSIGNED (resulttype));

  besti = -1;
  bestcost = BIG_ARBITRARY_NUMBER;

  for (i = 0; optab[i].opcode != -1; ++i)
    {
      cost = 0;
      DEDUCE_CONVERSION (arg0code, optab[i].arg0);
      cost += conversion_recipe[(int) arg0code][(int) optab[i].arg0].cost;
      DEDUCE_CONVERSION (arg1code, optab[i].arg1);
      cost += conversion_recipe[(int) arg1code][(int) optab[i].arg1].cost;
      if (cost < bestcost)
	{
	  besti = i;
	  bestcost = cost;
	}
    }

  if (besti == -1)
    abort ();

  expand_expr (arg1, 0, VOIDmode, 0);
  emit_typecode_conversion (arg1code, optab[besti].arg1);
  expand_expr (arg0, 0, VOIDmode, 0);
  emit_typecode_conversion (arg0code, optab[besti].arg0);
  bc_emit_instruction (optab[besti].opcode);
  emit_typecode_conversion (optab[besti].result, resultcode);
}

/* Emit an appropriate unary operation.  */
void
bc_expand_unary_operation (optab, resulttype, arg0)
     struct unary_operator optab[];
     tree resulttype, arg0;
{
  int i, besti, cost, bestcost;
  enum typecode resultcode, arg0code;
  
  resultcode = preferred_typecode (TYPE_MODE (resulttype), TREE_UNSIGNED (resulttype));
  arg0code = preferred_typecode (TYPE_MODE (TREE_TYPE (arg0)), TREE_UNSIGNED (TREE_TYPE (arg0)));

  besti = -1;
  bestcost = BIG_ARBITRARY_NUMBER;

  for (i = 0; optab[i].opcode != -1; ++i)
    {
      DEDUCE_CONVERSION (arg0code, optab[i].arg0);
      cost = conversion_recipe[(int) arg0code][(int) optab[i].arg0].cost;
      if (cost < bestcost)
	{
	  besti = i;
	  bestcost = cost;
	}
    }

  if (besti == -1)
    abort ();

  expand_expr (arg0, 0, VOIDmode, 0);
  emit_typecode_conversion (arg0code, optab[besti].arg0);
  bc_emit_instruction (optab[besti].opcode);
  emit_typecode_conversion (optab[besti].result, resultcode);
}


/* Emit an appropriate increment.  */
void
bc_expand_increment (optab, type)
     struct increment_operator optab[];
     tree type;
{
  enum typecode code;
  int i;

  code = preferred_typecode (TYPE_MODE (type), TREE_UNSIGNED (type));
  for (i = 0; (int) optab[i].opcode >= 0; ++i)
    if (code == optab[i].arg)
      {
	bc_emit_instruction (optab[i].opcode);
	return;
      }
  abort ();
}
