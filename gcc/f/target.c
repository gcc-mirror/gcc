/* target.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 1996, 1997, 1998, 2002 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

   Related Modules:
      None

   Description:
      Implements conversion of lexer tokens to machine-dependent numerical
      form and accordingly issues diagnostic messages when necessary.

      Also, this module, especially its .h file, provides nearly all of the
      information on the target machine's data type, kind type, and length
      type capabilities.  The idea is that by carefully going through
      target.h and changing things properly, one can accomplish much
      towards the porting of the FFE to a new machine.	There are limits
      to how much this can accomplish towards that end, however.  For one
      thing, the ffeexpr_collapse_convert function doesn't contain all the
      conversion cases necessary, because the text file would be
      enormous (even though most of the function would be cut during the
      cpp phase because of the absence of the types), so when adding to
      the number of supported kind types for a given type, one must look
      to see if ffeexpr_collapse_convert needs modification in this area,
      in addition to providing the appropriate macros and functions in
      ffetarget.  Note that if combinatorial explosion actually becomes a
      problem for a given machine, one might have to modify the way conversion
      expressions are built so that instead of just one conversion expr, a
      series of conversion exprs are built to make a path from one type to
      another that is not a "near neighbor".  For now, however, with a handful
      of each of the numeric types and only one character type, things appear
      manageable.

      A nonobvious change to ffetarget would be if the target machine was
      not a 2's-complement machine.  Any item with the word "magical" (case-
      insensitive) in the FFE's source code (at least) indicates an assumption
      that a 2's-complement machine is the target, and thus that there exists
      a magnitude that can be represented as a negative number but not as
      a positive number.  It is possible that this situation can be dealt
      with by changing only ffetarget, for example, on a 1's-complement
      machine, perhaps #defineing ffetarget_constant_is_magical to simply
      FALSE along with making the appropriate changes in ffetarget's number
      parsing functions would be sufficient to effectively "comment out" code
      in places like ffeexpr that do certain magical checks.  But it is
      possible there are other 2's-complement dependencies lurking in the
      FFE (as possibly is true of any large program); if you find any, please
      report them so we can replace them with dependencies on ffetarget
      instead.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "target.h"
#include "diagnostic.h"
#include "bad.h"
#include "info.h"
#include "lex.h"
#include "malloc.h"
#include "real.h"
#include "toplev.h"

/* Externals defined here. */

char ffetarget_string_[40];	/* Temp for ascii-to-double (atof). */
HOST_WIDE_INT ffetarget_long_val_;
HOST_WIDE_INT ffetarget_long_junk_;

/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */


/* Static functions (internal). */

static void ffetarget_print_char_ (FILE *f, unsigned char c);

/* Internal macros. */



/* ffetarget_print_char_ -- Print a single character (in apostrophe context)

   See prototype.

   Outputs char so it prints or is escaped C style.  */

static void
ffetarget_print_char_ (FILE *f, unsigned char c)
{
  switch (c)
    {
    case '\\':
      fputs ("\\\\", f);
      break;

    case '\'':
      fputs ("\\\'", f);
      break;

    default:
      if (ISPRINT (c))
	fputc (c, f);
      else
	fprintf (f, "\\%03o", (unsigned int) c);
      break;
    }
}

/* ffetarget_aggregate_info -- Determine type for aggregate storage area

   See prototype.

   If aggregate type is distinct, just return it.  Else return a type
   representing a common denominator for the nondistinct type (for now,
   just return default character, since that'll work on almost all target
   machines).

   The rules for abt/akt are (as implemented by ffestorag_update):

   abt == FFEINFO_basictypeANY (akt == FFEINFO_kindtypeANY also, by
   definition): CHARACTER and non-CHARACTER types mixed.

   abt == FFEINFO_basictypeNONE (akt == FFEINFO_kindtypeNONE also, by
   definition): More than one non-CHARACTER type mixed, but no CHARACTER
   types mixed in.

   abt some other value, akt == FFEINFO_kindtypeNONE: abt indicates the
   only basic type mixed in, but more than one kind type is mixed in.

   abt some other value, akt some other value: abt and akt indicate the
   only type represented in the aggregation.  */

void
ffetarget_aggregate_info (ffeinfoBasictype *ebt, ffeinfoKindtype *ekt,
			  ffetargetAlign *units, ffeinfoBasictype abt,
			  ffeinfoKindtype akt)
{
  ffetype type;

  if ((abt == FFEINFO_basictypeNONE) || (abt == FFEINFO_basictypeANY)
      || (akt == FFEINFO_kindtypeNONE))
    {
      *ebt = FFEINFO_basictypeCHARACTER;
      *ekt = FFEINFO_kindtypeCHARACTERDEFAULT;
    }
  else
    {
      *ebt = abt;
      *ekt = akt;
    }

  type = ffeinfo_type (*ebt, *ekt);
  assert (type != NULL);

  *units = ffetype_size (type);
}

/* ffetarget_align -- Align one storage area to superordinate, update super

   See prototype.

   updated_alignment/updated_modulo contain the already existing
   alignment requirements for the storage area at whose offset the
   object with alignment requirements alignment/modulo is to be placed.
   Find the smallest pad such that the requirements are maintained and
   return it, but only after updating the updated_alignment/_modulo
   requirements as necessary to indicate the placement of the new object.  */

ffetargetAlign
ffetarget_align (ffetargetAlign *updated_alignment,
		 ffetargetAlign *updated_modulo, ffetargetOffset offset,
		 ffetargetAlign alignment, ffetargetAlign modulo)
{
  ffetargetAlign pad;
  ffetargetAlign min_pad;	/* Minimum amount of padding needed. */
  ffetargetAlign min_m = 0;	/* Minimum-padding m. */
  ffetargetAlign ua;		/* Updated alignment. */
  ffetargetAlign um;		/* Updated modulo. */
  ffetargetAlign ucnt;		/* Multiplier applied to ua. */
  ffetargetAlign m;		/* Copy of modulo. */
  ffetargetAlign cnt;		/* Multiplier applied to alignment. */
  ffetargetAlign i;
  ffetargetAlign j;

  assert (alignment > 0);
  assert (*updated_alignment > 0);
  
  assert (*updated_modulo < *updated_alignment);
  assert (modulo < alignment);

  /* The easy case: similar alignment requirements.  */
  if (*updated_alignment == alignment)
    {
      if (modulo > *updated_modulo)
	pad = alignment - (modulo - *updated_modulo);
      else
	pad = *updated_modulo - modulo;
      if (offset < 0)
	/* De-negatize offset, since % wouldn't do the expected thing.  */
	offset = alignment - ((- offset) % alignment);
      pad = (offset + pad) % alignment;
      if (pad != 0)
	pad = alignment - pad;
      return pad;
    }

  /* Sigh, find LCM (Least Common Multiple) for the two alignment factors. */

  for (ua = *updated_alignment, ucnt = 1;
       ua % alignment != 0;
       ua += *updated_alignment)
    ++ucnt;

  cnt = ua / alignment;

  if (offset < 0)
    /* De-negatize offset, since % wouldn't do the expected thing.  */
    offset = ua - ((- offset) % ua);

  /* Set to largest value.  */
  min_pad = ~(ffetargetAlign) 0;

  /* Find all combinations of modulo values the two alignment requirements
     have; pick the combination that results in the smallest padding
     requirement.  Of course, if a zero-pad requirement is encountered, just
     use that one. */

  for (um = *updated_modulo, i = 0; i < ucnt; um += *updated_alignment, ++i)
    {
      for (m = modulo, j = 0; j < cnt; m += alignment, ++j)
	{
	  /* This code is similar to the "easy case" code above. */
	  if (m > um)
	    pad = ua - (m - um);
	  else
	    pad = um - m;
	  pad = (offset + pad) % ua;
	  if (pad == 0)
	    {
	      /* A zero pad means we've got something useful.  */
	      *updated_alignment = ua;
	      *updated_modulo = um;
	      return 0;
	    }
	  pad = ua - pad;
	  if (pad < min_pad)
	    {			/* New minimum padding value. */
	      min_pad = pad;
	      min_m = um;
	    }
	}
    }

  *updated_alignment = ua;
  *updated_modulo = min_m;
  return min_pad;
}

/* Always append a null byte to the end, in case this is wanted in
   a special case such as passing a string as a FORMAT or %REF.
   Done to save a bit of hassle, nothing more, but it's a kludge anyway,
   because it isn't a "feature" that is self-documenting.  Use the
   string "FFETARGET-NULL-KLUDGE" to flag anyplace you use this feature
   in the code.  */

#if FFETARGET_okCHARACTER1
bool
ffetarget_character1 (ffetargetCharacter1 *val, ffelexToken character,
		      mallocPool pool)
{
  val->length = ffelex_token_length (character);
  if (val->length == 0)
    val->text = NULL;
  else
    {
      val->text = malloc_new_kp (pool, "ffetargetCharacter1", val->length + 1);
      memcpy (val->text, ffelex_token_text (character), val->length);
      val->text[val->length] = '\0';
    }

  return TRUE;
}

#endif
/* Produce orderable comparison between two constants

   Compare lengths, if equal then use memcmp.  */

#if FFETARGET_okCHARACTER1
int
ffetarget_cmp_character1 (ffetargetCharacter1 l, ffetargetCharacter1 r)
{
  if (l.length < r.length)
    return -1;
  if (l.length > r.length)
    return 1;
  if (l.length == 0)
    return 0;
  return memcmp (l.text, r.text, l.length);
}

#endif
/* ffetarget_concatenate_character1 -- Perform CONCAT op on two constants

   Always append a null byte to the end, in case this is wanted in
   a special case such as passing a string as a FORMAT or %REF.
   Done to save a bit of hassle, nothing more, but it's a kludge anyway,
   because it isn't a "feature" that is self-documenting.  Use the
   string "FFETARGET-NULL-KLUDGE" to flag anyplace you use this feature
   in the code.  */

#if FFETARGET_okCHARACTER1
ffebad
ffetarget_concatenate_character1 (ffetargetCharacter1 *res,
	      ffetargetCharacter1 l, ffetargetCharacter1 r, mallocPool pool,
				  ffetargetCharacterSize *len)
{
  res->length = *len = l.length + r.length;
  if (*len == 0)
    res->text = NULL;
  else
    {
      res->text = malloc_new_kp (pool, "ffetargetCharacter1(CONCAT)", *len + 1);
      if (l.length != 0)
	memcpy (res->text, l.text, l.length);
      if (r.length != 0)
	memcpy (res->text + l.length, r.text, r.length);
      res->text[*len] = '\0';
    }

  return FFEBAD;
}

#endif
/* ffetarget_eq_character1 -- Perform relational comparison on char constants

   Compare lengths, if equal then use memcmp.  */

#if FFETARGET_okCHARACTER1
ffebad
ffetarget_eq_character1 (bool *res, ffetargetCharacter1 l,
			 ffetargetCharacter1 r)
{
  assert (l.length == r.length);
  *res = (memcmp (l.text, r.text, l.length) == 0);
  return FFEBAD;
}

#endif
/* ffetarget_le_character1 -- Perform relational comparison on char constants

   Compare lengths, if equal then use memcmp.  */

#if FFETARGET_okCHARACTER1
ffebad
ffetarget_le_character1 (bool *res, ffetargetCharacter1 l,
			 ffetargetCharacter1 r)
{
  assert (l.length == r.length);
  *res = (memcmp (l.text, r.text, l.length) <= 0);
  return FFEBAD;
}

#endif
/* ffetarget_lt_character1 -- Perform relational comparison on char constants

   Compare lengths, if equal then use memcmp.  */

#if FFETARGET_okCHARACTER1
ffebad
ffetarget_lt_character1 (bool *res, ffetargetCharacter1 l,
			 ffetargetCharacter1 r)
{
  assert (l.length == r.length);
  *res = (memcmp (l.text, r.text, l.length) < 0);
  return FFEBAD;
}

#endif
/* ffetarget_ge_character1 -- Perform relational comparison on char constants

   Compare lengths, if equal then use memcmp.  */

#if FFETARGET_okCHARACTER1
ffebad
ffetarget_ge_character1 (bool *res, ffetargetCharacter1 l,
			 ffetargetCharacter1 r)
{
  assert (l.length == r.length);
  *res = (memcmp (l.text, r.text, l.length) >= 0);
  return FFEBAD;
}

#endif
/* ffetarget_gt_character1 -- Perform relational comparison on char constants

   Compare lengths, if equal then use memcmp.  */

#if FFETARGET_okCHARACTER1
ffebad
ffetarget_gt_character1 (bool *res, ffetargetCharacter1 l,
			 ffetargetCharacter1 r)
{
  assert (l.length == r.length);
  *res = (memcmp (l.text, r.text, l.length) > 0);
  return FFEBAD;
}
#endif

#if FFETARGET_okCHARACTER1
bool
ffetarget_iszero_character1 (ffetargetCharacter1 constant)
{
  ffetargetCharacterSize i;

  for (i = 0; i < constant.length; ++i)
    if (constant.text[i] != 0)
      return FALSE;
  return TRUE;
}
#endif

bool
ffetarget_iszero_hollerith (ffetargetHollerith constant)
{
  ffetargetHollerithSize i;

  for (i = 0; i < constant.length; ++i)
    if (constant.text[i] != 0)
      return FALSE;
  return TRUE;
}

/* ffetarget_layout -- Do storage requirement analysis for entity

   Return the alignment/modulo requirements along with the size, given the
   data type info and the number of elements an array (1 for a scalar).	 */

void
ffetarget_layout (const char *error_text UNUSED, ffetargetAlign *alignment,
		  ffetargetAlign *modulo, ffetargetOffset *size,
		  ffeinfoBasictype bt, ffeinfoKindtype kt,
		  ffetargetCharacterSize charsize,
		  ffetargetIntegerDefault num_elements)
{
  bool ok;			/* For character type. */
  ffetargetOffset numele;	/* Converted from num_elements. */
  ffetype type;

  type = ffeinfo_type (bt, kt);
  assert (type != NULL);

  *alignment = ffetype_alignment (type);
  *modulo = ffetype_modulo (type);
  if (bt == FFEINFO_basictypeCHARACTER)
    {
      ok = ffetarget_offset_charsize (size, charsize, ffetype_size (type));
#ifdef ffetarget_offset_overflow
      if (!ok)
	ffetarget_offset_overflow (error_text);
#endif
    }
  else
    *size = ffetype_size (type);

  if ((num_elements < 0)
      || !ffetarget_offset (&numele, num_elements)
      || !ffetarget_offset_multiply (size, *size, numele))
    {
      ffetarget_offset_overflow (error_text);
      *alignment = 1;
      *modulo = 0;
      *size = 0;
    }
}

/* ffetarget_ne_character1 -- Perform relational comparison on char constants

   Compare lengths, if equal then use memcmp.  */

#if FFETARGET_okCHARACTER1
ffebad
ffetarget_ne_character1 (bool *res, ffetargetCharacter1 l,
			 ffetargetCharacter1 r)
{
  assert (l.length == r.length);
  *res = (memcmp (l.text, r.text, l.length) != 0);
  return FFEBAD;
}

#endif
/* ffetarget_substr_character1 -- Perform SUBSTR op on three constants

   Always append a null byte to the end, in case this is wanted in
   a special case such as passing a string as a FORMAT or %REF.
   Done to save a bit of hassle, nothing more, but it's a kludge anyway,
   because it isn't a "feature" that is self-documenting.  Use the
   string "FFETARGET-NULL-KLUDGE" to flag anyplace you use this feature
   in the code.  */

#if FFETARGET_okCHARACTER1
ffebad
ffetarget_substr_character1 (ffetargetCharacter1 *res,
			     ffetargetCharacter1 l,
			     ffetargetCharacterSize first,
			     ffetargetCharacterSize last, mallocPool pool,
			     ffetargetCharacterSize *len)
{
  if (last < first)
    {
      res->length = *len = 0;
      res->text = NULL;
    }
  else
    {
      res->length = *len = last - first + 1;
      res->text = malloc_new_kp (pool, "ffetargetCharacter1(SUBSTR)", *len + 1);
      memcpy (res->text, l.text + first - 1, *len);
      res->text[*len] = '\0';
    }

  return FFEBAD;
}

#endif
/* ffetarget_cmp_hollerith -- Produce orderable comparison between two
   constants

   Compare lengths, if equal then use memcmp.  */

int
ffetarget_cmp_hollerith (ffetargetHollerith l, ffetargetHollerith r)
{
  if (l.length < r.length)
    return -1;
  if (l.length > r.length)
    return 1;
  return memcmp (l.text, r.text, l.length);
}

ffebad
ffetarget_convert_any_character1_ (char *res, size_t size,
				   ffetargetCharacter1 l)
{
  if (size <= (size_t) l.length)
    {
      char *p;
      ffetargetCharacterSize i;

      memcpy (res, l.text, size);
      for (p = &l.text[0] + size, i = l.length - size;
	   i > 0;
	   ++p, --i)
	if (*p != ' ')
	  return FFEBAD_TRUNCATING_CHARACTER;
    }
  else
    {
      memcpy (res, l.text, size);
      memset (res + l.length, ' ', size - l.length);
    }

  return FFEBAD;
}

ffebad
ffetarget_convert_any_hollerith_ (char *res, size_t size,
				  ffetargetHollerith l)
{
  if (size <= (size_t) l.length)
    {
      char *p;
      ffetargetCharacterSize i;

      memcpy (res, l.text, size);
      for (p = &l.text[0] + size, i = l.length - size;
	   i > 0;
	   ++p, --i)
	if (*p != ' ')
	  return FFEBAD_TRUNCATING_HOLLERITH;
    }
  else
    {
      memcpy (res, l.text, size);
      memset (res + l.length, ' ', size - l.length);
    }

  return FFEBAD;
}

ffebad
ffetarget_convert_any_typeless_ (char *res, size_t size,
				 ffetargetTypeless l)
{
  unsigned long long int l1;
  unsigned long int l2;
  unsigned int l3;
  unsigned short int l4;
  unsigned char l5;
  size_t size_of;
  char *p;

  if (size >= sizeof (l1))
    {
      l1 = l;
      p = (char *) &l1;
      size_of = sizeof (l1);
    }
  else if (size >= sizeof (l2))
    {
      l2 = l;
      p = (char *) &l2;
      size_of = sizeof (l2);
      l1 = l2;
    }
  else if (size >= sizeof (l3))
    {
      l3 = l;
      p = (char *) &l3;
      size_of = sizeof (l3);
      l1 = l3;
    }
  else if (size >= sizeof (l4))
    {
      l4 = l;
      p = (char *) &l4;
      size_of = sizeof (l4);
      l1 = l4;
    }
  else if (size >= sizeof (l5))
    {
      l5 = l;
      p = (char *) &l5;
      size_of = sizeof (l5);
      l1 = l5;
    }
  else
    {
      assert ("stumped by conversion from typeless!" == NULL);
      abort ();
    }

  if (size <= size_of)
    {
      int i = size_of - size;

      memcpy (res, p + i, size);
      for (; i > 0; ++p, --i)
	if (*p != '\0')
	  return FFEBAD_TRUNCATING_TYPELESS;
    }
  else
    {
      int i = size - size_of;

      memset (res, 0, i);
      memcpy (res + i, p, size_of);
    }

  if (l1 != l)
    return FFEBAD_TRUNCATING_TYPELESS;
  return FFEBAD;
}

/* Always append a null byte to the end, in case this is wanted in
   a special case such as passing a string as a FORMAT or %REF.
   Done to save a bit of hassle, nothing more, but it's a kludge anyway,
   because it isn't a "feature" that is self-documenting.  Use the
   string "FFETARGET-NULL-KLUDGE" to flag anyplace you use this feature
   in the code.  */

#if FFETARGET_okCHARACTER1
ffebad
ffetarget_convert_character1_character1 (ffetargetCharacter1 *res,
					 ffetargetCharacterSize size,
					 ffetargetCharacter1 l,
					 mallocPool pool)
{
  res->length = size;
  if (size == 0)
    res->text = NULL;
  else
    {
      res->text = malloc_new_kp (pool, "FFETARGET cvt char1", size + 1);
      if (size <= l.length)
	memcpy (res->text, l.text, size);
      else
	{
	  memcpy (res->text, l.text, l.length);
	  memset (res->text + l.length, ' ', size - l.length);
	}
      res->text[size] = '\0';
    }

  return FFEBAD;
}

#endif

/* Always append a null byte to the end, in case this is wanted in
   a special case such as passing a string as a FORMAT or %REF.
   Done to save a bit of hassle, nothing more, but it's a kludge anyway,
   because it isn't a "feature" that is self-documenting.  Use the
   string "FFETARGET-NULL-KLUDGE" to flag anyplace you use this feature
   in the code.  */

#if FFETARGET_okCHARACTER1
ffebad
ffetarget_convert_character1_hollerith (ffetargetCharacter1 *res,
					ffetargetCharacterSize size,
					ffetargetHollerith l, mallocPool pool)
{
  res->length = size;
  if (size == 0)
    res->text = NULL;
  else
    {
      res->text = malloc_new_kp (pool, "FFETARGET cvt char1", size + 1);
      res->text[size] = '\0';
      if (size <= l.length)
	{
	  char *p;
	  ffetargetCharacterSize i;

	  memcpy (res->text, l.text, size);
	  for (p = &l.text[0] + size, i = l.length - size;
	       i > 0;
	       ++p, --i)
	    if (*p != ' ')
	      return FFEBAD_TRUNCATING_HOLLERITH;
	}
      else
	{
	  memcpy (res->text, l.text, l.length);
	  memset (res->text + l.length, ' ', size - l.length);
	}
    }

  return FFEBAD;
}

#endif
/* ffetarget_convert_character1_integer4 -- Raw conversion.

   Always append a null byte to the end, in case this is wanted in
   a special case such as passing a string as a FORMAT or %REF.
   Done to save a bit of hassle, nothing more, but it's a kludge anyway,
   because it isn't a "feature" that is self-documenting.  Use the
   string "FFETARGET-NULL-KLUDGE" to flag anyplace you use this feature
   in the code.  */

#if FFETARGET_okCHARACTER1
ffebad
ffetarget_convert_character1_integer4 (ffetargetCharacter1 *res,
				       ffetargetCharacterSize size,
				       ffetargetInteger4 l, mallocPool pool)
{
  long long int l1;
  long int l2;
  int l3;
  short int l4;
  char l5;
  size_t size_of;
  char *p;

  if (((size_t) size) >= sizeof (l1))
    {
      l1 = l;
      p = (char *) &l1;
      size_of = sizeof (l1);
    }
  else if (((size_t) size) >= sizeof (l2))
    {
      l2 = l;
      p = (char *) &l2;
      size_of = sizeof (l2);
      l1 = l2;
    }
  else if (((size_t) size) >= sizeof (l3))
    {
      l3 = l;
      p = (char *) &l3;
      size_of = sizeof (l3);
      l1 = l3;
    }
  else if (((size_t) size) >= sizeof (l4))
    {
      l4 = l;
      p = (char *) &l4;
      size_of = sizeof (l4);
      l1 = l4;
    }
  else if (((size_t) size) >= sizeof (l5))
    {
      l5 = l;
      p = (char *) &l5;
      size_of = sizeof (l5);
      l1 = l5;
    }
  else
    {
      assert ("stumped by conversion from integer1!" == NULL);
      abort ();
    }

  res->length = size;
  if (size == 0)
    res->text = NULL;
  else
    {
      res->text = malloc_new_kp (pool, "FFETARGET cvt char1", size + 1);
      res->text[size] = '\0';
      if (((size_t) size) <= size_of)
	{
	  int i = size_of - size;

	  memcpy (res->text, p + i, size);
	  for (; i > 0; ++p, --i)
	    if (*p != 0)
	      return FFEBAD_TRUNCATING_NUMERIC;
	}
      else
	{
	  int i = size - size_of;

	  memset (res->text, 0, i);
	  memcpy (res->text + i, p, size_of);
	}
    }

  if (l1 != l)
    return FFEBAD_TRUNCATING_NUMERIC;
  return FFEBAD;
}

#endif
/* ffetarget_convert_character1_logical4 -- Raw conversion.

   Always append a null byte to the end, in case this is wanted in
   a special case such as passing a string as a FORMAT or %REF.
   Done to save a bit of hassle, nothing more, but it's a kludge anyway,
   because it isn't a "feature" that is self-documenting.  Use the
   string "FFETARGET-NULL-KLUDGE" to flag anyplace you use this feature
   in the code.  */

#if FFETARGET_okCHARACTER1
ffebad
ffetarget_convert_character1_logical4 (ffetargetCharacter1 *res,
				       ffetargetCharacterSize size,
				       ffetargetLogical4 l, mallocPool pool)
{
  long long int l1;
  long int l2;
  int l3;
  short int l4;
  char l5;
  size_t size_of;
  char *p;

  if (((size_t) size) >= sizeof (l1))
    {
      l1 = l;
      p = (char *) &l1;
      size_of = sizeof (l1);
    }
  else if (((size_t) size) >= sizeof (l2))
    {
      l2 = l;
      p = (char *) &l2;
      size_of = sizeof (l2);
      l1 = l2;
    }
  else if (((size_t) size) >= sizeof (l3))
    {
      l3 = l;
      p = (char *) &l3;
      size_of = sizeof (l3);
      l1 = l3;
    }
  else if (((size_t) size) >= sizeof (l4))
    {
      l4 = l;
      p = (char *) &l4;
      size_of = sizeof (l4);
      l1 = l4;
    }
  else if (((size_t) size) >= sizeof (l5))
    {
      l5 = l;
      p = (char *) &l5;
      size_of = sizeof (l5);
      l1 = l5;
    }
  else
    {
      assert ("stumped by conversion from logical1!" == NULL);
      abort ();
    }

  res->length = size;
  if (size == 0)
    res->text = NULL;
  else
    {
      res->text = malloc_new_kp (pool, "FFETARGET cvt char1", size + 1);
      res->text[size] = '\0';
      if (((size_t) size) <= size_of)
	{
	  int i = size_of - size;

	  memcpy (res->text, p + i, size);
	  for (; i > 0; ++p, --i)
	    if (*p != 0)
	      return FFEBAD_TRUNCATING_NUMERIC;
	}
      else
	{
	  int i = size - size_of;

	  memset (res->text, 0, i);
	  memcpy (res->text + i, p, size_of);
	}
    }

  if (l1 != l)
    return FFEBAD_TRUNCATING_NUMERIC;
  return FFEBAD;
}

#endif
/* ffetarget_convert_character1_typeless -- Raw conversion.

   Always append a null byte to the end, in case this is wanted in
   a special case such as passing a string as a FORMAT or %REF.
   Done to save a bit of hassle, nothing more, but it's a kludge anyway,
   because it isn't a "feature" that is self-documenting.  Use the
   string "FFETARGET-NULL-KLUDGE" to flag anyplace you use this feature
   in the code.  */

#if FFETARGET_okCHARACTER1
ffebad
ffetarget_convert_character1_typeless (ffetargetCharacter1 *res,
				       ffetargetCharacterSize size,
				       ffetargetTypeless l, mallocPool pool)
{
  unsigned long long int l1;
  unsigned long int l2;
  unsigned int l3;
  unsigned short int l4;
  unsigned char l5;
  size_t size_of;
  char *p;

  if (((size_t) size) >= sizeof (l1))
    {
      l1 = l;
      p = (char *) &l1;
      size_of = sizeof (l1);
    }
  else if (((size_t) size) >= sizeof (l2))
    {
      l2 = l;
      p = (char *) &l2;
      size_of = sizeof (l2);
      l1 = l2;
    }
  else if (((size_t) size) >= sizeof (l3))
    {
      l3 = l;
      p = (char *) &l3;
      size_of = sizeof (l3);
      l1 = l3;
    }
  else if (((size_t) size) >= sizeof (l4))
    {
      l4 = l;
      p = (char *) &l4;
      size_of = sizeof (l4);
      l1 = l4;
    }
  else if (((size_t) size) >= sizeof (l5))
    {
      l5 = l;
      p = (char *) &l5;
      size_of = sizeof (l5);
      l1 = l5;
    }
  else
    {
      assert ("stumped by conversion from typeless!" == NULL);
      abort ();
    }

  res->length = size;
  if (size == 0)
    res->text = NULL;
  else
    {
      res->text = malloc_new_kp (pool, "FFETARGET cvt char1", size + 1);
      res->text[size] = '\0';
      if (((size_t) size) <= size_of)
	{
	  int i = size_of - size;

	  memcpy (res->text, p + i, size);
	  for (; i > 0; ++p, --i)
	    if (*p != 0)
	      return FFEBAD_TRUNCATING_TYPELESS;
	}
      else
	{
	  int i = size - size_of;

	  memset (res->text, 0, i);
	  memcpy (res->text + i, p, size_of);
	}
    }

  if (l1 != l)
    return FFEBAD_TRUNCATING_TYPELESS;
  return FFEBAD;
}

#endif
/* ffetarget_divide_complex1 -- Divide function

   See prototype.  */

#if FFETARGET_okCOMPLEX1
ffebad
ffetarget_divide_complex1 (ffetargetComplex1 *res, ffetargetComplex1 l,
			   ffetargetComplex1 r)
{
  ffebad bad;
  ffetargetReal1 tmp1, tmp2, tmp3, tmp4;

  bad = ffetarget_multiply_real1 (&tmp1, r.real, r.real);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_multiply_real1 (&tmp2, r.imaginary, r.imaginary);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_add_real1 (&tmp3, tmp1, tmp2);
  if (bad != FFEBAD)
    return bad;

  if (ffetarget_iszero_real1 (tmp3))
    {
      ffetarget_real1_zero (&(res)->real);
      ffetarget_real1_zero (&(res)->imaginary);
      return FFEBAD_DIV_BY_ZERO;
    }

  bad = ffetarget_multiply_real1 (&tmp1, l.real, r.real);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_multiply_real1 (&tmp2, l.imaginary, r.imaginary);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_add_real1 (&tmp4, tmp1, tmp2);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_divide_real1 (&res->real, tmp4, tmp3);
  if (bad != FFEBAD)
    return bad;

  bad = ffetarget_multiply_real1 (&tmp1, r.real, l.imaginary);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_multiply_real1 (&tmp2, l.real, r.imaginary);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_subtract_real1 (&tmp4, tmp1, tmp2);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_divide_real1 (&res->imaginary, tmp4, tmp3);

  return FFEBAD;
}

#endif
/* ffetarget_divide_complex2 -- Divide function

   See prototype.  */

#if FFETARGET_okCOMPLEX2
ffebad
ffetarget_divide_complex2 (ffetargetComplex2 *res, ffetargetComplex2 l,
			   ffetargetComplex2 r)
{
  ffebad bad;
  ffetargetReal2 tmp1, tmp2, tmp3, tmp4;

  bad = ffetarget_multiply_real2 (&tmp1, r.real, r.real);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_multiply_real2 (&tmp2, r.imaginary, r.imaginary);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_add_real2 (&tmp3, tmp1, tmp2);
  if (bad != FFEBAD)
    return bad;

  if (ffetarget_iszero_real2 (tmp3))
    {
      ffetarget_real2_zero (&(res)->real);
      ffetarget_real2_zero (&(res)->imaginary);
      return FFEBAD_DIV_BY_ZERO;
    }

  bad = ffetarget_multiply_real2 (&tmp1, l.real, r.real);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_multiply_real2 (&tmp2, l.imaginary, r.imaginary);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_add_real2 (&tmp4, tmp1, tmp2);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_divide_real2 (&res->real, tmp4, tmp3);
  if (bad != FFEBAD)
    return bad;

  bad = ffetarget_multiply_real2 (&tmp1, r.real, l.imaginary);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_multiply_real2 (&tmp2, l.real, r.imaginary);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_subtract_real2 (&tmp4, tmp1, tmp2);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_divide_real2 (&res->imaginary, tmp4, tmp3);

  return FFEBAD;
}

#endif
/* ffetarget_hollerith -- Convert token to a hollerith constant

   Always append a null byte to the end, in case this is wanted in
   a special case such as passing a string as a FORMAT or %REF.
   Done to save a bit of hassle, nothing more, but it's a kludge anyway,
   because it isn't a "feature" that is self-documenting.  Use the
   string "FFETARGET-NULL-KLUDGE" to flag anyplace you use this feature
   in the code.  */

bool
ffetarget_hollerith (ffetargetHollerith *val, ffelexToken integer,
		     mallocPool pool)
{
  val->length = ffelex_token_length (integer);
  val->text = malloc_new_kp (pool, "ffetargetHollerith", val->length + 1);
  memcpy (val->text, ffelex_token_text (integer), val->length);
  val->text[val->length] = '\0';

  return TRUE;
}

/* ffetarget_integer_bad_magical -- Complain about a magical number

   Just calls ffebad with the arguments.  */

void
ffetarget_integer_bad_magical (ffelexToken t)
{
  ffebad_start (FFEBAD_BAD_MAGICAL);
  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
  ffebad_finish ();
}

/* ffetarget_integer_bad_magical_binary -- Complain about a magical number

   Just calls ffebad with the arguments.  */

void
ffetarget_integer_bad_magical_binary (ffelexToken integer,
				      ffelexToken minus)
{
  ffebad_start (FFEBAD_BAD_MAGICAL_BINARY);
  ffebad_here (0, ffelex_token_where_line (integer),
	       ffelex_token_where_column (integer));
  ffebad_here (1, ffelex_token_where_line (minus),
	       ffelex_token_where_column (minus));
  ffebad_finish ();
}

/* ffetarget_integer_bad_magical_precedence -- Complain about a magical
						   number

   Just calls ffebad with the arguments.  */

void
ffetarget_integer_bad_magical_precedence (ffelexToken integer,
					  ffelexToken uminus,
					  ffelexToken higher_op)
{
  ffebad_start (FFEBAD_BAD_MAGICAL_PRECEDENCE);
  ffebad_here (0, ffelex_token_where_line (integer),
	       ffelex_token_where_column (integer));
  ffebad_here (1, ffelex_token_where_line (uminus),
	       ffelex_token_where_column (uminus));
  ffebad_here (2, ffelex_token_where_line (higher_op),
	       ffelex_token_where_column (higher_op));
  ffebad_finish ();
}

/* ffetarget_integer_bad_magical_precedence_binary -- Complain...

   Just calls ffebad with the arguments.  */

void
ffetarget_integer_bad_magical_precedence_binary (ffelexToken integer,
						 ffelexToken minus,
						 ffelexToken higher_op)
{
  ffebad_start (FFEBAD_BAD_MAGICAL_PRECEDENCE_BINARY);
  ffebad_here (0, ffelex_token_where_line (integer),
	       ffelex_token_where_column (integer));
  ffebad_here (1, ffelex_token_where_line (minus),
	       ffelex_token_where_column (minus));
  ffebad_here (2, ffelex_token_where_line (higher_op),
	       ffelex_token_where_column (higher_op));
  ffebad_finish ();
}

/* ffetarget_integer1 -- Convert token to an integer

   See prototype.

   Token use count not affected overall.  */

#if FFETARGET_okINTEGER1
bool
ffetarget_integer1 (ffetargetInteger1 *val, ffelexToken integer)
{
  ffetargetInteger1 x;
  char *p;
  char c;

  assert (ffelex_token_type (integer) == FFELEX_typeNUMBER);

  p = ffelex_token_text (integer);
  x = 0;

  /* Skip past leading zeros. */

  while (((c = *p) != '\0') && (c == '0'))
    ++p;

  /* Interpret rest of number. */

  while (c != '\0')
    {
      if ((x == FFETARGET_integerALMOST_BIG_MAGICAL)
	  && (c == '0' + FFETARGET_integerFINISH_BIG_MAGICAL)
	  && (*(p + 1) == '\0'))
	{
	  *val = (ffetargetInteger1) FFETARGET_integerBIG_MAGICAL;
	  return TRUE;
	}
      else if (x == FFETARGET_integerALMOST_BIG_MAGICAL)
	{
	  if ((c > '0' + FFETARGET_integerFINISH_BIG_MAGICAL)
	      || (*(p + 1) != '\0'))
	    {
	      ffebad_start (FFEBAD_INTEGER_TOO_LARGE);
	      ffebad_here (0, ffelex_token_where_line (integer),
			   ffelex_token_where_column (integer));
	      ffebad_finish ();
	      *val = 0;
	      return FALSE;
	    }
	}
      else if (x > FFETARGET_integerALMOST_BIG_MAGICAL)
	{
	  ffebad_start (FFEBAD_INTEGER_TOO_LARGE);
	  ffebad_here (0, ffelex_token_where_line (integer),
		       ffelex_token_where_column (integer));
	  ffebad_finish ();
	  *val = 0;
	  return FALSE;
	}
      x = x * 10 + c - '0';
      c = *(++p);
    };

  *val = x;
  return TRUE;
}

#endif
/* ffetarget_integerbinary -- Convert token to a binary integer

   ffetarget_integerbinary x;
   if (ffetarget_integerdefault_8(&x,integer_token))
       // conversion ok.

   Token use count not affected overall.  */

bool
ffetarget_integerbinary (ffetargetIntegerDefault *val, ffelexToken integer)
{
  ffetargetIntegerDefault x;
  char *p;
  char c;
  bool bad_digit;

  assert ((ffelex_token_type (integer) == FFELEX_typeNAME)
	  || (ffelex_token_type (integer) == FFELEX_typeNUMBER));

  p = ffelex_token_text (integer);
  x = 0;

  /* Skip past leading zeros. */

  while (((c = *p) != '\0') && (c == '0'))
    ++p;

  /* Interpret rest of number. */

  bad_digit = FALSE;
  while (c != '\0')
    {
      if ((c >= '0') && (c <= '1'))
	c -= '0';
      else
	{
	  bad_digit = TRUE;
	  c = 0;
	}

#if 0				/* Don't complain about signed overflow; just
				   unsigned overflow. */
      if ((x == FFETARGET_integerALMOST_BIG_OVERFLOW_BINARY)
	  && (c == FFETARGET_integerFINISH_BIG_OVERFLOW_BINARY)
	  && (*(p + 1) == '\0'))
	{
	  *val = FFETARGET_integerBIG_OVERFLOW_BINARY;
	  return TRUE;
	}
      else
#endif
#if FFETARGET_integerFINISH_BIG_OVERFLOW_BINARY == 0
      if ((x & FFETARGET_integerALMOST_BIG_OVERFLOW_BINARY) != 0)
#else
      if (x == FFETARGET_integerALMOST_BIG_OVERFLOW_BINARY)
	{
	  if ((c > FFETARGET_integerFINISH_BIG_OVERFLOW_BINARY)
	      || (*(p + 1) != '\0'))
	    {
	      ffebad_start (FFEBAD_INTEGER_TOO_LARGE);
	      ffebad_here (0, ffelex_token_where_line (integer),
			   ffelex_token_where_column (integer));
	      ffebad_finish ();
	      *val = 0;
	      return FALSE;
	    }
	}
      else if (x > FFETARGET_integerALMOST_BIG_OVERFLOW_BINARY)
#endif
	{
	  ffebad_start (FFEBAD_INTEGER_TOO_LARGE);
	  ffebad_here (0, ffelex_token_where_line (integer),
		       ffelex_token_where_column (integer));
	  ffebad_finish ();
	  *val = 0;
	  return FALSE;
	}
      x = (x << 1) + c;
      c = *(++p);
    };

  if (bad_digit)
    {
      ffebad_start (FFEBAD_INVALID_BINARY_DIGIT);
      ffebad_here (0, ffelex_token_where_line (integer),
		   ffelex_token_where_column (integer));
      ffebad_finish ();
    }

  *val = x;
  return !bad_digit;
}

/* ffetarget_integerhex -- Convert token to a hex integer

   ffetarget_integerhex x;
   if (ffetarget_integerdefault_8(&x,integer_token))
       // conversion ok.

   Token use count not affected overall.  */

bool
ffetarget_integerhex (ffetargetIntegerDefault *val, ffelexToken integer)
{
  ffetargetIntegerDefault x;
  char *p;
  char c;
  bool bad_digit;

  assert ((ffelex_token_type (integer) == FFELEX_typeNAME)
	  || (ffelex_token_type (integer) == FFELEX_typeNUMBER));

  p = ffelex_token_text (integer);
  x = 0;

  /* Skip past leading zeros. */

  while (((c = *p) != '\0') && (c == '0'))
    ++p;

  /* Interpret rest of number. */

  bad_digit = FALSE;
  while (c != '\0')
    {
      if (hex_p (c))
	c = hex_value (c);
      else
	{
	  bad_digit = TRUE;
	  c = 0;
	}

#if 0				/* Don't complain about signed overflow; just
				   unsigned overflow. */
      if ((x == FFETARGET_integerALMOST_BIG_OVERFLOW_HEX)
	  && (c == FFETARGET_integerFINISH_BIG_OVERFLOW_HEX)
	  && (*(p + 1) == '\0'))
	{
	  *val = FFETARGET_integerBIG_OVERFLOW_HEX;
	  return TRUE;
	}
      else
#endif
#if FFETARGET_integerFINISH_BIG_OVERFLOW_HEX == 0
      if (x >= FFETARGET_integerALMOST_BIG_OVERFLOW_HEX)
#else
      if (x == FFETARGET_integerALMOST_BIG_OVERFLOW_HEX)
	{
	  if ((c > FFETARGET_integerFINISH_BIG_OVERFLOW_HEX)
	      || (*(p + 1) != '\0'))
	    {
	      ffebad_start (FFEBAD_INTEGER_TOO_LARGE);
	      ffebad_here (0, ffelex_token_where_line (integer),
			   ffelex_token_where_column (integer));
	      ffebad_finish ();
	      *val = 0;
	      return FALSE;
	    }
	}
      else if (x > FFETARGET_integerALMOST_BIG_OVERFLOW_HEX)
#endif
	{
	  ffebad_start (FFEBAD_INTEGER_TOO_LARGE);
	  ffebad_here (0, ffelex_token_where_line (integer),
		       ffelex_token_where_column (integer));
	  ffebad_finish ();
	  *val = 0;
	  return FALSE;
	}
      x = (x << 4) + c;
      c = *(++p);
    };

  if (bad_digit)
    {
      ffebad_start (FFEBAD_INVALID_HEX_DIGIT);
      ffebad_here (0, ffelex_token_where_line (integer),
		   ffelex_token_where_column (integer));
      ffebad_finish ();
    }

  *val = x;
  return !bad_digit;
}

/* ffetarget_integeroctal -- Convert token to an octal integer

   ffetarget_integeroctal x;
   if (ffetarget_integerdefault_8(&x,integer_token))
       // conversion ok.

   Token use count not affected overall.  */

bool
ffetarget_integeroctal (ffetargetIntegerDefault *val, ffelexToken integer)
{
  ffetargetIntegerDefault x;
  char *p;
  char c;
  bool bad_digit;

  assert ((ffelex_token_type (integer) == FFELEX_typeNAME)
	  || (ffelex_token_type (integer) == FFELEX_typeNUMBER));

  p = ffelex_token_text (integer);
  x = 0;

  /* Skip past leading zeros. */

  while (((c = *p) != '\0') && (c == '0'))
    ++p;

  /* Interpret rest of number. */

  bad_digit = FALSE;
  while (c != '\0')
    {
      if ((c >= '0') && (c <= '7'))
	c -= '0';
      else
	{
	  bad_digit = TRUE;
	  c = 0;
	}

#if 0				/* Don't complain about signed overflow; just
				   unsigned overflow. */
      if ((x == FFETARGET_integerALMOST_BIG_OVERFLOW_OCTAL)
	  && (c == FFETARGET_integerFINISH_BIG_OVERFLOW_OCTAL)
	  && (*(p + 1) == '\0'))
	{
	  *val = FFETARGET_integerBIG_OVERFLOW_OCTAL;
	  return TRUE;
	}
      else
#endif
#if FFETARGET_integerFINISH_BIG_OVERFLOW_OCTAL == 0
      if (x >= FFETARGET_integerALMOST_BIG_OVERFLOW_OCTAL)
#else
      if (x == FFETARGET_integerALMOST_BIG_OVERFLOW_OCTAL)
	{
	  if ((c > FFETARGET_integerFINISH_BIG_OVERFLOW_OCTAL)
	      || (*(p + 1) != '\0'))
	    {
	      ffebad_start (FFEBAD_INTEGER_TOO_LARGE);
	      ffebad_here (0, ffelex_token_where_line (integer),
			   ffelex_token_where_column (integer));
	      ffebad_finish ();
	      *val = 0;
	      return FALSE;
	    }
	}
      else if (x > FFETARGET_integerALMOST_BIG_OVERFLOW_OCTAL)
#endif
	{
	  ffebad_start (FFEBAD_INTEGER_TOO_LARGE);
	  ffebad_here (0, ffelex_token_where_line (integer),
		       ffelex_token_where_column (integer));
	  ffebad_finish ();
	  *val = 0;
	  return FALSE;
	}
      x = (x << 3) + c;
      c = *(++p);
    };

  if (bad_digit)
    {
      ffebad_start (FFEBAD_INVALID_OCTAL_DIGIT);
      ffebad_here (0, ffelex_token_where_line (integer),
		   ffelex_token_where_column (integer));
      ffebad_finish ();
    }

  *val = x;
  return !bad_digit;
}

/* ffetarget_multiply_complex1 -- Multiply function

   See prototype.  */

#if FFETARGET_okCOMPLEX1
ffebad
ffetarget_multiply_complex1 (ffetargetComplex1 *res, ffetargetComplex1 l,
			     ffetargetComplex1 r)
{
  ffebad bad;
  ffetargetReal1 tmp1, tmp2;

  bad = ffetarget_multiply_real1 (&tmp1, l.real, r.real);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_multiply_real1 (&tmp2, l.imaginary, r.imaginary);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_subtract_real1 (&res->real, tmp1, tmp2);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_multiply_real1 (&tmp1, l.imaginary, r.real);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_multiply_real1 (&tmp2, l.real, r.imaginary);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_add_real1 (&res->imaginary, tmp1, tmp2);

  return bad;
}

#endif
/* ffetarget_multiply_complex2 -- Multiply function

   See prototype.  */

#if FFETARGET_okCOMPLEX2
ffebad
ffetarget_multiply_complex2 (ffetargetComplex2 *res, ffetargetComplex2 l,
			     ffetargetComplex2 r)
{
  ffebad bad;
  ffetargetReal2 tmp1, tmp2;

  bad = ffetarget_multiply_real2 (&tmp1, l.real, r.real);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_multiply_real2 (&tmp2, l.imaginary, r.imaginary);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_subtract_real2 (&res->real, tmp1, tmp2);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_multiply_real2 (&tmp1, l.imaginary, r.real);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_multiply_real2 (&tmp2, l.real, r.imaginary);
  if (bad != FFEBAD)
    return bad;
  bad = ffetarget_add_real2 (&res->imaginary, tmp1, tmp2);

  return bad;
}

#endif
/* ffetarget_power_complexdefault_integerdefault -- Power function

   See prototype.  */

ffebad
ffetarget_power_complexdefault_integerdefault (ffetargetComplexDefault *res,
					       ffetargetComplexDefault l,
					       ffetargetIntegerDefault r)
{
  ffebad bad;
  ffetargetRealDefault tmp;
  ffetargetRealDefault tmp1;
  ffetargetRealDefault tmp2;
  ffetargetRealDefault two;

  if (ffetarget_iszero_real1 (l.real)
      && ffetarget_iszero_real1 (l.imaginary))
    {
      ffetarget_real1_zero (&res->real);
      ffetarget_real1_zero (&res->imaginary);
      return FFEBAD;
    }

  if (r == 0)
    {
      ffetarget_real1_one (&res->real);
      ffetarget_real1_zero (&res->imaginary);
      return FFEBAD;
    }

  if (r < 0)
    {
      r = -r;
      bad = ffetarget_multiply_real1 (&tmp1, l.real, l.real);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real1 (&tmp2, l.imaginary, l.imaginary);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_add_real1 (&tmp, tmp1, tmp2);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_divide_real1 (&l.real, l.real, tmp);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_divide_real1 (&l.imaginary, l.imaginary, tmp);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_uminus_real1 (&l.imaginary, l.imaginary);
      if (bad != FFEBAD)
	return bad;
    }

  ffetarget_real1_two (&two);

  while ((r & 1) == 0)
    {
      bad = ffetarget_multiply_real1 (&tmp1, l.real, l.real);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real1 (&tmp2, l.imaginary, l.imaginary);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_subtract_real1 (&tmp, tmp1, tmp2);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real1 (&l.imaginary, l.real, l.imaginary);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real1 (&l.imaginary, l.imaginary, two);
      if (bad != FFEBAD)
	return bad;
      l.real = tmp;
      r >>= 1;
    }

  *res = l;
  r >>= 1;

  while (r != 0)
    {
      bad = ffetarget_multiply_real1 (&tmp1, l.real, l.real);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real1 (&tmp2, l.imaginary, l.imaginary);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_subtract_real1 (&tmp, tmp1, tmp2);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real1 (&l.imaginary, l.real, l.imaginary);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real1 (&l.imaginary, l.imaginary, two);
      if (bad != FFEBAD)
	return bad;
      l.real = tmp;
      if ((r & 1) == 1)
	{
	  bad = ffetarget_multiply_real1 (&tmp1, res->real, l.real);
	  if (bad != FFEBAD)
	    return bad;
	  bad = ffetarget_multiply_real1 (&tmp2, res->imaginary,
					  l.imaginary);
	  if (bad != FFEBAD)
	    return bad;
	  bad = ffetarget_subtract_real1 (&tmp, tmp1, tmp2);
	  if (bad != FFEBAD)
	    return bad;
	  bad = ffetarget_multiply_real1 (&tmp1, res->imaginary, l.real);
	  if (bad != FFEBAD)
	    return bad;
	  bad = ffetarget_multiply_real1 (&tmp2, res->real, l.imaginary);
	  if (bad != FFEBAD)
	    return bad;
	  bad = ffetarget_add_real1 (&res->imaginary, tmp1, tmp2);
	  if (bad != FFEBAD)
	    return bad;
	  res->real = tmp;
	}
      r >>= 1;
    }

  return FFEBAD;
}

/* ffetarget_power_complexdouble_integerdefault -- Power function

   See prototype.  */

#if FFETARGET_okCOMPLEXDOUBLE
ffebad
ffetarget_power_complexdouble_integerdefault (ffetargetComplexDouble *res,
			ffetargetComplexDouble l, ffetargetIntegerDefault r)
{
  ffebad bad;
  ffetargetRealDouble tmp;
  ffetargetRealDouble tmp1;
  ffetargetRealDouble tmp2;
  ffetargetRealDouble two;

  if (ffetarget_iszero_real2 (l.real)
      && ffetarget_iszero_real2 (l.imaginary))
    {
      ffetarget_real2_zero (&res->real);
      ffetarget_real2_zero (&res->imaginary);
      return FFEBAD;
    }

  if (r == 0)
    {
      ffetarget_real2_one (&res->real);
      ffetarget_real2_zero (&res->imaginary);
      return FFEBAD;
    }

  if (r < 0)
    {
      r = -r;
      bad = ffetarget_multiply_real2 (&tmp1, l.real, l.real);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real2 (&tmp2, l.imaginary, l.imaginary);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_add_real2 (&tmp, tmp1, tmp2);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_divide_real2 (&l.real, l.real, tmp);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_divide_real2 (&l.imaginary, l.imaginary, tmp);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_uminus_real2 (&l.imaginary, l.imaginary);
      if (bad != FFEBAD)
	return bad;
    }

  ffetarget_real2_two (&two);

  while ((r & 1) == 0)
    {
      bad = ffetarget_multiply_real2 (&tmp1, l.real, l.real);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real2 (&tmp2, l.imaginary, l.imaginary);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_subtract_real2 (&tmp, tmp1, tmp2);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real2 (&l.imaginary, l.real, l.imaginary);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real2 (&l.imaginary, l.imaginary, two);
      if (bad != FFEBAD)
	return bad;
      l.real = tmp;
      r >>= 1;
    }

  *res = l;
  r >>= 1;

  while (r != 0)
    {
      bad = ffetarget_multiply_real2 (&tmp1, l.real, l.real);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real2 (&tmp2, l.imaginary, l.imaginary);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_subtract_real2 (&tmp, tmp1, tmp2);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real2 (&l.imaginary, l.real, l.imaginary);
      if (bad != FFEBAD)
	return bad;
      bad = ffetarget_multiply_real2 (&l.imaginary, l.imaginary, two);
      if (bad != FFEBAD)
	return bad;
      l.real = tmp;
      if ((r & 1) == 1)
	{
	  bad = ffetarget_multiply_real2 (&tmp1, res->real, l.real);
	  if (bad != FFEBAD)
	    return bad;
	  bad = ffetarget_multiply_real2 (&tmp2, res->imaginary,
					  l.imaginary);
	  if (bad != FFEBAD)
	    return bad;
	  bad = ffetarget_subtract_real2 (&tmp, tmp1, tmp2);
	  if (bad != FFEBAD)
	    return bad;
	  bad = ffetarget_multiply_real2 (&tmp1, res->imaginary, l.real);
	  if (bad != FFEBAD)
	    return bad;
	  bad = ffetarget_multiply_real2 (&tmp2, res->real, l.imaginary);
	  if (bad != FFEBAD)
	    return bad;
	  bad = ffetarget_add_real2 (&res->imaginary, tmp1, tmp2);
	  if (bad != FFEBAD)
	    return bad;
	  res->real = tmp;
	}
      r >>= 1;
    }

  return FFEBAD;
}

#endif
/* ffetarget_power_integerdefault_integerdefault -- Power function

   See prototype.  */

ffebad
ffetarget_power_integerdefault_integerdefault (ffetargetIntegerDefault *res,
		       ffetargetIntegerDefault l, ffetargetIntegerDefault r)
{
  if (l == 0)
    {
      *res = 0;
      return FFEBAD;
    }

  if (r == 0)
    {
      *res = 1;
      return FFEBAD;
    }

  if (r < 0)
    {
      if (l == 1)
	*res = 1;
      else if (l == 0)
	*res = 1;
      else if (l == -1)
	*res = ((-r) & 1) == 0 ? 1 : -1;
      else
	*res = 0;
      return FFEBAD;
    }

  while ((r & 1) == 0)
    {
      l *= l;
      r >>= 1;
    }

  *res = l;
  r >>= 1;

  while (r != 0)
    {
      l *= l;
      if ((r & 1) == 1)
	*res *= l;
      r >>= 1;
    }

  return FFEBAD;
}

/* ffetarget_power_realdefault_integerdefault -- Power function

   See prototype.  */

ffebad
ffetarget_power_realdefault_integerdefault (ffetargetRealDefault *res,
			  ffetargetRealDefault l, ffetargetIntegerDefault r)
{
  ffebad bad;

  if (ffetarget_iszero_real1 (l))
    {
      ffetarget_real1_zero (res);
      return FFEBAD;
    }

  if (r == 0)
    {
      ffetarget_real1_one (res);
      return FFEBAD;
    }

  if (r < 0)
    {
      ffetargetRealDefault one;

      ffetarget_real1_one (&one);
      r = -r;
      bad = ffetarget_divide_real1 (&l, one, l);
      if (bad != FFEBAD)
	return bad;
    }

  while ((r & 1) == 0)
    {
      bad = ffetarget_multiply_real1 (&l, l, l);
      if (bad != FFEBAD)
	return bad;
      r >>= 1;
    }

  *res = l;
  r >>= 1;

  while (r != 0)
    {
      bad = ffetarget_multiply_real1 (&l, l, l);
      if (bad != FFEBAD)
	return bad;
      if ((r & 1) == 1)
	{
	  bad = ffetarget_multiply_real1 (res, *res, l);
	  if (bad != FFEBAD)
	    return bad;
	}
      r >>= 1;
    }

  return FFEBAD;
}

/* ffetarget_power_realdouble_integerdefault -- Power function

   See prototype.  */

ffebad
ffetarget_power_realdouble_integerdefault (ffetargetRealDouble *res,
					   ffetargetRealDouble l,
					   ffetargetIntegerDefault r)
{
  ffebad bad;

  if (ffetarget_iszero_real2 (l))
    {
      ffetarget_real2_zero (res);
      return FFEBAD;
    }

  if (r == 0)
    {
      ffetarget_real2_one (res);
      return FFEBAD;
    }

  if (r < 0)
    {
      ffetargetRealDouble one;

      ffetarget_real2_one (&one);
      r = -r;
      bad = ffetarget_divide_real2 (&l, one, l);
      if (bad != FFEBAD)
	return bad;
    }

  while ((r & 1) == 0)
    {
      bad = ffetarget_multiply_real2 (&l, l, l);
      if (bad != FFEBAD)
	return bad;
      r >>= 1;
    }

  *res = l;
  r >>= 1;

  while (r != 0)
    {
      bad = ffetarget_multiply_real2 (&l, l, l);
      if (bad != FFEBAD)
	return bad;
      if ((r & 1) == 1)
	{
	  bad = ffetarget_multiply_real2 (res, *res, l);
	  if (bad != FFEBAD)
	    return bad;
	}
      r >>= 1;
    }

  return FFEBAD;
}

/* ffetarget_print_binary -- Output typeless binary integer

   ffetargetTypeless val;
   ffetarget_typeless_binary(dmpout,val);  */

void
ffetarget_print_binary (FILE *f, ffetargetTypeless value)
{
  char *p;
  char digits[sizeof (value) * CHAR_BIT + 1];

  if (f == NULL)
    f = dmpout;

  p = &digits[ARRAY_SIZE (digits) - 1];
  *p = '\0';
  do
    {
      *--p = (value & 1) + '0';
      value >>= 1;
    } while (value == 0);

  fputs (p, f);
}

/* ffetarget_print_character1 -- Output character string

   ffetargetCharacter1 val;
   ffetarget_print_character1(dmpout,val);  */

void
ffetarget_print_character1 (FILE *f, ffetargetCharacter1 value)
{
  unsigned char *p;
  ffetargetCharacterSize i;

  fputc ('\'', dmpout);
  for (i = 0, p = value.text; i < value.length; ++i, ++p)
    ffetarget_print_char_ (f, *p);
  fputc ('\'', dmpout);
}

/* ffetarget_print_hollerith -- Output hollerith string

   ffetargetHollerith val;
   ffetarget_print_hollerith(dmpout,val);  */

void
ffetarget_print_hollerith (FILE *f, ffetargetHollerith value)
{
  unsigned char *p;
  ffetargetHollerithSize i;

  fputc ('\'', dmpout);
  for (i = 0, p = value.text; i < value.length; ++i, ++p)
    ffetarget_print_char_ (f, *p);
  fputc ('\'', dmpout);
}

/* ffetarget_print_octal -- Output typeless octal integer

   ffetargetTypeless val;
   ffetarget_print_octal(dmpout,val);  */

void
ffetarget_print_octal (FILE *f, ffetargetTypeless value)
{
  char *p;
  char digits[sizeof (value) * CHAR_BIT / 3 + 1];

  if (f == NULL)
    f = dmpout;

  p = &digits[ARRAY_SIZE (digits) - 3];
  *p = '\0';
  do
    {
      *--p = (value & 3) + '0';
      value >>= 3;
    } while (value == 0);

  fputs (p, f);
}

/* ffetarget_print_hex -- Output typeless hex integer

   ffetargetTypeless val;
   ffetarget_print_hex(dmpout,val);  */

void
ffetarget_print_hex (FILE *f, ffetargetTypeless value)
{
  char *p;
  char digits[sizeof (value) * CHAR_BIT / 4 + 1];
  static const char hexdigits[16] = "0123456789ABCDEF";

  if (f == NULL)
    f = dmpout;

  p = &digits[ARRAY_SIZE (digits) - 3];
  *p = '\0';
  do
    {
      *--p = hexdigits[value & 4];
      value >>= 4;
    } while (value == 0);

  fputs (p, f);
}

/* ffetarget_real1 -- Convert token to a single-precision real number

   See prototype.

   Pass NULL for any token not provided by the user, but a valid Fortran
   real number must be provided somehow.  For example, it is ok for
   exponent_sign_token and exponent_digits_token to be NULL as long as
   exponent_token not only starts with "E" or "e" but also contains at least
   one digit following it.  Token use counts not affected overall.  */

#if FFETARGET_okREAL1
bool
ffetarget_real1 (ffetargetReal1 *value, ffelexToken integer,
		 ffelexToken decimal, ffelexToken fraction,
		 ffelexToken exponent, ffelexToken exponent_sign,
		 ffelexToken exponent_digits)
{
  size_t sz = 1;		/* Allow room for '\0' byte at end. */
  char *ptr = &ffetarget_string_[0];
  char *p = ptr;
  char *q;

#define dotok(x) if (x != NULL) ++sz;
#define dotoktxt(x) if (x != NULL) sz += ffelex_token_length(x)

  dotoktxt (integer);
  dotok (decimal);
  dotoktxt (fraction);
  dotoktxt (exponent);
  dotok (exponent_sign);
  dotoktxt (exponent_digits);

#undef dotok
#undef dotoktxt

  if (sz > ARRAY_SIZE (ffetarget_string_))
    p = ptr = malloc_new_ks (malloc_pool_image (), "ffetarget_real1", sz);

#define dotoktxt(x) if (x != NULL)				   \
		  {						   \
		  for (q = ffelex_token_text(x); *q != '\0'; ++q)  \
		    *p++ = *q;					   \
		  }

  dotoktxt (integer);

  if (decimal != NULL)
    *p++ = '.';

  dotoktxt (fraction);
  dotoktxt (exponent);

  if (exponent_sign != NULL)
    {
      if (ffelex_token_type (exponent_sign) == FFELEX_typePLUS)
	*p++ = '+';
      else
	{
	  assert (ffelex_token_type (exponent_sign) == FFELEX_typeMINUS);
	  *p++ = '-';
	}
    }

  dotoktxt (exponent_digits);

#undef dotoktxt

  *p = '\0';

  {
    REAL_VALUE_TYPE rv;
    real_from_string (&rv, ptr);
    ffetarget_make_real1 (value, rv);
  }

  if (sz > ARRAY_SIZE (ffetarget_string_))
    malloc_kill_ks (malloc_pool_image (), ptr, sz);

  return TRUE;
}

#endif
/* ffetarget_real2 -- Convert token to a single-precision real number

   See prototype.

   Pass NULL for any token not provided by the user, but a valid Fortran
   real number must be provided somehow.  For example, it is ok for
   exponent_sign_token and exponent_digits_token to be NULL as long as
   exponent_token not only starts with "E" or "e" but also contains at least
   one digit following it.  Token use counts not affected overall.  */

#if FFETARGET_okREAL2
bool
ffetarget_real2 (ffetargetReal2 *value, ffelexToken integer,
		 ffelexToken decimal, ffelexToken fraction,
		 ffelexToken exponent, ffelexToken exponent_sign,
		 ffelexToken exponent_digits)
{
  size_t sz = 1;		/* Allow room for '\0' byte at end. */
  char *ptr = &ffetarget_string_[0];
  char *p = ptr;
  char *q;

#define dotok(x) if (x != NULL) ++sz;
#define dotoktxt(x) if (x != NULL) sz += ffelex_token_length(x)

  dotoktxt (integer);
  dotok (decimal);
  dotoktxt (fraction);
  dotoktxt (exponent);
  dotok (exponent_sign);
  dotoktxt (exponent_digits);

#undef dotok
#undef dotoktxt

  if (sz > ARRAY_SIZE (ffetarget_string_))
    p = ptr = malloc_new_ks (malloc_pool_image (), "ffetarget_real1", sz);

#define dotoktxt(x) if (x != NULL)				   \
		  {						   \
		  for (q = ffelex_token_text(x); *q != '\0'; ++q)  \
		    *p++ = *q;					   \
		  }
#define dotoktxtexp(x) if (x != NULL)				       \
		  {						       \
		  *p++ = 'E';					       \
		  for (q = ffelex_token_text(x) + 1; *q != '\0'; ++q)  \
		    *p++ = *q;					       \
		  }

  dotoktxt (integer);

  if (decimal != NULL)
    *p++ = '.';

  dotoktxt (fraction);
  dotoktxtexp (exponent);

  if (exponent_sign != NULL)
    {
      if (ffelex_token_type (exponent_sign) == FFELEX_typePLUS)
	*p++ = '+';
      else
	{
	  assert (ffelex_token_type (exponent_sign) == FFELEX_typeMINUS);
	  *p++ = '-';
	}
    }

  dotoktxt (exponent_digits);

#undef dotoktxt

  *p = '\0';

  {
    REAL_VALUE_TYPE rv;
    real_from_string (&rv, ptr);
    ffetarget_make_real2 (value, rv);
  }

  if (sz > ARRAY_SIZE (ffetarget_string_))
    malloc_kill_ks (malloc_pool_image (), ptr, sz);

  return TRUE;
}

#endif
bool
ffetarget_typeless_binary (ffetargetTypeless *xvalue, ffelexToken token)
{
  char *p;
  char c;
  ffetargetTypeless value = 0;
  ffetargetTypeless new_value = 0;
  bool bad_digit = FALSE;
  bool overflow = FALSE;

  p = ffelex_token_text (token);

  for (c = *p; c != '\0'; c = *++p)
    {
      new_value <<= 1;
      if ((new_value >> 1) != value)
	overflow = TRUE;
      if (ISDIGIT (c))
	new_value += c - '0';
      else
	bad_digit = TRUE;
      value = new_value;
    }

  if (bad_digit)
    {
      ffebad_start (FFEBAD_INVALID_TYPELESS_BINARY_DIGIT);
      ffebad_here (0, ffelex_token_where_line (token),
		   ffelex_token_where_column (token));
      ffebad_finish ();
    }
  else if (overflow)
    {
      ffebad_start (FFEBAD_TYPELESS_OVERFLOW);
      ffebad_here (0, ffelex_token_where_line (token),
		   ffelex_token_where_column (token));
      ffebad_finish ();
    }

  *xvalue = value;

  return !bad_digit && !overflow;
}

bool
ffetarget_typeless_octal (ffetargetTypeless *xvalue, ffelexToken token)
{
  char *p;
  char c;
  ffetargetTypeless value = 0;
  ffetargetTypeless new_value = 0;
  bool bad_digit = FALSE;
  bool overflow = FALSE;

  p = ffelex_token_text (token);

  for (c = *p; c != '\0'; c = *++p)
    {
      new_value <<= 3;
      if ((new_value >> 3) != value)
	overflow = TRUE;
      if (ISDIGIT (c))
	new_value += c - '0';
      else
	bad_digit = TRUE;
      value = new_value;
    }

  if (bad_digit)
    {
      ffebad_start (FFEBAD_INVALID_TYPELESS_OCTAL_DIGIT);
      ffebad_here (0, ffelex_token_where_line (token),
		   ffelex_token_where_column (token));
      ffebad_finish ();
    }
  else if (overflow)
    {
      ffebad_start (FFEBAD_TYPELESS_OVERFLOW);
      ffebad_here (0, ffelex_token_where_line (token),
		   ffelex_token_where_column (token));
      ffebad_finish ();
    }

  *xvalue = value;

  return !bad_digit && !overflow;
}

bool
ffetarget_typeless_hex (ffetargetTypeless *xvalue, ffelexToken token)
{
  char *p;
  char c;
  ffetargetTypeless value = 0;
  ffetargetTypeless new_value = 0;
  bool bad_digit = FALSE;
  bool overflow = FALSE;

  p = ffelex_token_text (token);

  for (c = *p; c != '\0'; c = *++p)
    {
      new_value <<= 4;
      if ((new_value >> 4) != value)
	overflow = TRUE;
      if (hex_p (c))
	new_value += hex_value (c);
      else
	bad_digit = TRUE;
      value = new_value;
    }

  if (bad_digit)
    {
      ffebad_start (FFEBAD_INVALID_TYPELESS_HEX_DIGIT);
      ffebad_here (0, ffelex_token_where_line (token),
		   ffelex_token_where_column (token));
      ffebad_finish ();
    }
  else if (overflow)
    {
      ffebad_start (FFEBAD_TYPELESS_OVERFLOW);
      ffebad_here (0, ffelex_token_where_line (token),
		   ffelex_token_where_column (token));
      ffebad_finish ();
    }

  *xvalue = value;

  return !bad_digit && !overflow;
}

void
ffetarget_verify_character1 (mallocPool pool, ffetargetCharacter1 val)
{
  if (val.length != 0)
    malloc_verify_kp (pool, val.text, val.length);
}

/* This is like memcpy.	 It is needed because some systems' header files
   don't declare memcpy as a function but instead
   "#define memcpy(to,from,len) something".  */

void *
ffetarget_memcpy_ (void *dst, void *src, size_t len)
{
#ifdef CROSS_COMPILE
  /* HOST_WORDS_BIG_ENDIAN corresponds to both WORDS_BIG_ENDIAN and
     BYTES_BIG_ENDIAN (i.e. there are no HOST_ macros to represent a
     difference in the two latter).  */
  int host_words_big_endian =
#ifndef HOST_WORDS_BIG_ENDIAN
    0
#else
    HOST_WORDS_BIG_ENDIAN
#endif
    ;

  /* This is just hands thrown up in the air over bits coming through this
     function representing a number being memcpy:d as-is from host to
     target.  We can't generally adjust endianness here since we don't
     know whether it's an integer or floating point number; they're passed
     differently.  Better to not emit code at all than to emit wrong code.
     We will get some false hits because some data coming through here
     seems to be just character vectors, but often enough it's numbers,
     for instance in g77.f-torture/execute/980628-[4-6].f and alpha2.f.
     Still, we compile *some* code.  FIXME: Rewrite handling of numbers.  */
  if (!WORDS_BIG_ENDIAN != !host_words_big_endian
      || !BYTES_BIG_ENDIAN != !host_words_big_endian)
    sorry ("data initializer on host with different endianness");

#endif /* CROSS_COMPILE */

  return (void *) memcpy (dst, src, len);
}

/* ffetarget_num_digits_ -- Determine number of non-space characters in token

   ffetarget_num_digits_(token);

   All non-spaces are assumed to be binary, octal, or hex digits.  */

int
ffetarget_num_digits_ (ffelexToken token)
{
  int i;
  char *c;

  switch (ffelex_token_type (token))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNUMBER:
      return ffelex_token_length (token);

    case FFELEX_typeCHARACTER:
      i = 0;
      for (c = ffelex_token_text (token); *c != '\0'; ++c)
	{
	  if (*c != ' ')
	    ++i;
	}
      return i;

    default:
      assert ("weird token" == NULL);
      return 1;
    }
}
