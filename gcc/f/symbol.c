/* Implementation of Fortran symbol manager
   Copyright (C) 1995-1997 Free Software Foundation, Inc.
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
02111-1307, USA.  */

#include "proj.h"
#include "symbol.h"
#include "bad.h"
#include "bld.h"
#include "com.h"
#include "equiv.h"
#include "global.h"
#include "info.h"
#include "intrin.h"
#include "lex.h"
#include "malloc.h"
#include "src.h"
#include "st.h"
#include "storag.h"
#include "target.h"
#include "where.h"

/* Choice of how to handle global symbols -- either global only within the
   program unit being defined or global within the entire source file.
   The former is appropriate for systems where an object file can
   easily be taken apart program unit by program unit, the latter is the
   UNIX/C model where the object file is essentially a monolith.  */

#define FFESYMBOL_globalPROGUNIT_ 1
#define FFESYMBOL_globalFILE_ 2

/* Choose how to handle global symbols here.  */

#if FFECOM_targetCURRENT == FFECOM_targetFFE
#define FFESYMBOL_globalCURRENT_ FFESYMBOL_globalPROGUNIT_
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
/* Would be good to understand why PROGUNIT in this case too.
   (1995-08-22).  */
#define FFESYMBOL_globalCURRENT_ FFESYMBOL_globalPROGUNIT_
#else
#error
#endif

/* Choose how to handle memory pools based on global symbol stuff.  */

#if FFESYMBOL_globalCURRENT_ == FFESYMBOL_globalPROGUNIT_
#define FFESYMBOL_SPACE_POOL_ ffe_pool_program_unit()
#elif FFESYMBOL_globalCURRENT_ == FFESYMBOL_globalFILE_
#define FFESYMBOL_SPACE_POOL_ ffe_pool_file()
#else
#error
#endif

/* What kind of retraction is needed for a symbol?  */

enum _ffesymbol_retractcommand_
  {
    FFESYMBOL_retractcommandDELETE_,
    FFESYMBOL_retractcommandRETRACT_,
    FFESYMBOL_retractcommand_
  };
typedef enum _ffesymbol_retractcommand_ ffesymbolRetractCommand_;

/* This object keeps track of retraction for a symbol and links to the next
   such object.  */

typedef struct _ffesymbol_retract_ *ffesymbolRetract_;
struct _ffesymbol_retract_
  {
    ffesymbolRetract_ next;
    ffesymbolRetractCommand_ command;
    ffesymbol live;		/* Live symbol. */
    ffesymbol symbol;		/* Backup copy of symbol. */
  };

static ffebad ffesymbol_check_token_ (ffelexToken t, char *c);
static void ffesymbol_kill_manifest_ (void);
static ffesymbol ffesymbol_new_ (ffename n);
static ffesymbol ffesymbol_unhook_ (ffesymbol s);
static void ffesymbol_whine_state_ (ffebad bad, ffelexToken t, char c);

/* Manifest names for unnamed things (as tokens) so we make them only
   once.  */

static ffelexToken ffesymbol_token_blank_common_ = NULL;
static ffelexToken ffesymbol_token_unnamed_main_ = NULL;
static ffelexToken ffesymbol_token_unnamed_blockdata_ = NULL;

/* Name spaces currently in force.  */

static ffenameSpace ffesymbol_global_ = NULL;
static ffenameSpace ffesymbol_local_ = NULL;
static ffenameSpace ffesymbol_sfunc_ = NULL;

/* Keep track of retraction.  */

static bool ffesymbol_retractable_ = FALSE;
static mallocPool ffesymbol_retract_pool_;
static ffesymbolRetract_ ffesymbol_retract_first_;
static ffesymbolRetract_ *ffesymbol_retract_list_;

/* List of state names. */

static const char *ffesymbol_state_name_[] =
{
  "?",
  "@",
  "&",
  "$",
};

/* List of attribute names. */

static const char *ffesymbol_attr_name_[] =
{
#define DEFATTR(ATTR,ATTRS,NAME) NAME,
#include "symbol.def"
#undef DEFATTR
};


/* Check whether the token text has any invalid characters.  If not,
   return FALSE.  If so, if error messages inhibited, return TRUE
   so caller knows to try again later, else report error and return
   FALSE.  */

static ffebad
ffesymbol_check_token_ (ffelexToken t, char *c)
{
  char *p = ffelex_token_text (t);
  ffeTokenLength len = ffelex_token_length (t);
  ffebad bad;
  ffeTokenLength i = 0;
  ffebad skip_me = ((ffe_case_symbol () == FFE_caseINITCAP)
		    ? FFEBAD_SYMBOL_NOLOWER_INITCAP : FFEBAD + 1);
  ffebad stop_me = ((ffe_case_symbol () == FFE_caseINITCAP)
		    ? FFEBAD : FFEBAD + 1);
  if (len == 0)
    return FFEBAD;

  bad = ffesrc_bad_char_symbol_init (*p);
  if (bad == FFEBAD)
    {
      for (++i, ++p; i < len; ++i, ++p)
	{
	  bad = ffesrc_bad_char_symbol_noninit (*p);
	  if (bad == skip_me)
	    continue;		/* Keep looking for good InitCap character. */
	  if (bad == stop_me)
	    break;		/* Found good InitCap character. */
	  if (bad != FFEBAD)
	    break;		/* Bad character found. */
	}
    }

  if (bad != FFEBAD)
    {
      if (i >= len)
	*c = *(ffelex_token_text (t));
      else
	*c = *p;
    }

  return bad;
}

/* Kill manifest (g77-picked) names.  */

static void
ffesymbol_kill_manifest_ ()
{
  if (ffesymbol_token_blank_common_ != NULL)
    ffelex_token_kill (ffesymbol_token_blank_common_);
  if (ffesymbol_token_unnamed_main_ != NULL)
    ffelex_token_kill (ffesymbol_token_unnamed_main_);
  if (ffesymbol_token_unnamed_blockdata_ != NULL)
    ffelex_token_kill (ffesymbol_token_unnamed_blockdata_);

  ffesymbol_token_blank_common_ = NULL;
  ffesymbol_token_unnamed_main_ = NULL;
  ffesymbol_token_unnamed_blockdata_ = NULL;
}

/* Make new symbol.

   If the "retractable" flag is not set, just return the new symbol.
   Else, add symbol to the "retract" list as a delete item, set
   the "have_old" flag, and return the new symbol.  */

static ffesymbol
ffesymbol_new_ (ffename n)
{
  ffesymbol s;
  ffesymbolRetract_ r;

  assert (n != NULL);

  s = (ffesymbol) malloc_new_ks (FFESYMBOL_SPACE_POOL_, "FFESYMBOL",
				 sizeof (*s));
  s->name = n;
  s->other_space_name = NULL;
#if FFEGLOBAL_ENABLED
  s->global = NULL;
#endif
  s->attrs = FFESYMBOL_attrsetNONE;
  s->state = FFESYMBOL_stateNONE;
  s->info = ffeinfo_new_null ();
  s->dims = NULL;
  s->extents = NULL;
  s->dim_syms = NULL;
  s->array_size = NULL;
  s->init = NULL;
  s->accretion = NULL;
  s->accretes = 0;
  s->dummy_args = NULL;
  s->namelist = NULL;
  s->common_list = NULL;
  s->sfunc_expr = NULL;
  s->list_bottom = NULL;
  s->common = NULL;
  s->equiv = NULL;
  s->storage = NULL;
#ifdef FFECOM_symbolHOOK
  s->hook = FFECOM_symbolNULL;
#endif
  s->sfa_dummy_parent = NULL;
  s->func_result = NULL;
  s->value = 0;
  s->check_state = FFESYMBOL_checkstateNONE_;
  s->check_token = NULL;
  s->max_entry_num = 0;
  s->num_entries = 0;
  s->generic = FFEINTRIN_genNONE;
  s->specific = FFEINTRIN_specNONE;
  s->implementation = FFEINTRIN_impNONE;
  s->is_save = FALSE;
  s->is_init = FALSE;
  s->do_iter = FALSE;
  s->reported = FALSE;
  s->explicit_where = FALSE;
  s->namelisted = FALSE;
  s->assigned = FALSE;

  ffename_set_symbol (n, s);

  if (!ffesymbol_retractable_)
    {
      s->have_old = FALSE;
      return s;
    }

  r = (ffesymbolRetract_) malloc_new_kp (ffesymbol_retract_pool_,
					 "FFESYMBOL retract", sizeof (*r));
  r->next = NULL;
  r->command = FFESYMBOL_retractcommandDELETE_;
  r->live = s;
  r->symbol = NULL;		/* No backup copy. */

  *ffesymbol_retract_list_ = r;
  ffesymbol_retract_list_ = &r->next;

  s->have_old = TRUE;
  return s;
}

/* Unhook a symbol from its (soon-to-be-killed) name obj.

   NULLify the names to which this symbol points.  Do other cleanup as
   needed.  */

static ffesymbol
ffesymbol_unhook_ (ffesymbol s)
{
  s->other_space_name = s->name = NULL;
  if ((ffesymbol_attrs (s) & FFESYMBOL_attrsCBLOCK)
      || (ffesymbol_kind (s) == FFEINFO_kindNAMELIST))
    ffebld_end_list (ffesymbol_ptr_to_listbottom (s));
  if (s->check_state == FFESYMBOL_checkstatePENDING_)
    ffelex_token_kill (s->check_token);

  return s;
}

/* Issue diagnostic about bad character in token representing user-defined
   symbol name.	 */

static void
ffesymbol_whine_state_ (ffebad bad, ffelexToken t, char c)
{
  char badstr[2];

  badstr[0] = c;
  badstr[1] = '\0';

  ffebad_start (bad);
  ffebad_here (0, ffelex_token_where_line (t),
	       ffelex_token_where_column (t));
  ffebad_string (badstr);
  ffebad_finish ();
}

/* Returns a string representing the attributes set.  */

const char *
ffesymbol_attrs_string (ffesymbolAttrs attrs)
{
  static char string[FFESYMBOL_attr * 12 + 20];
  char *p;
  ffesymbolAttr attr;

  p = &string[0];

  if (attrs == FFESYMBOL_attrsetNONE)
    {
      strcpy (p, "NONE");
      return &string[0];
    }

  for (attr = 0; attr < FFESYMBOL_attr; ++attr)
    {
      if (attrs & ((ffesymbolAttrs) 1 << attr))
	{
	  attrs &= ~((ffesymbolAttrs) 1 << attr);
	  strcpy (p, ffesymbol_attr_name_[attr]);
	  while (*p)
	    ++p;
	  *(p++) = '|';
	}
    }
  if (attrs == FFESYMBOL_attrsetNONE)
    *--p = '\0';
  else
    sprintf (p, "?0x%" ffesymbolAttrs_f "x?", attrs);
  assert (((size_t) (p - &string[0])) < ARRAY_SIZE (string));
  return &string[0];
}

/* Check symbol's name for validity, considering that it might actually
   be an intrinsic and thus should not be complained about just yet.  */

void
ffesymbol_check (ffesymbol s, ffelexToken t, bool maybe_intrin)
{
  char c;
  ffebad bad;
  ffeintrinGen gen;
  ffeintrinSpec spec;
  ffeintrinImp imp;

  if (!ffesrc_check_symbol ()
      || ((s->check_state != FFESYMBOL_checkstateNONE_)
	  && ((s->check_state != FFESYMBOL_checkstateINHIBITED_)
	      || ffebad_inhibit ())))
    return;

  bad = ffesymbol_check_token_ (t, &c);

  if (bad == FFEBAD)
    {
      s->check_state = FFESYMBOL_checkstateCHECKED_;
      return;
    }

  if (maybe_intrin
      && ffeintrin_is_intrinsic (ffelex_token_text (t), NULL, FALSE,
				 &gen, &spec, &imp))
    {
      s->check_state = FFESYMBOL_checkstatePENDING_;
      s->check_token = ffelex_token_use (t);
      return;
    }

  if (ffebad_inhibit ())
    {
      s->check_state = FFESYMBOL_checkstateINHIBITED_;
      return;			/* Don't complain now, do it later. */
    }

  s->check_state = FFESYMBOL_checkstateCHECKED_;

  ffesymbol_whine_state_ (bad, t, c);
}

/* Declare a BLOCKDATA unit.

   Retrieves or creates the ffesymbol for the specified BLOCKDATA (unnamed
   if t is NULL).  Doesn't actually ensure the named item is a
   BLOCKDATA; the caller must handle that.  */

ffesymbol
ffesymbol_declare_blockdataunit (ffelexToken t, ffewhereLine wl,
				 ffewhereColumn wc)
{
  ffename n;
  ffesymbol s;
  bool user = (t != NULL);

  assert (!ffesymbol_retractable_);

  if (t == NULL)
    {
      if (ffesymbol_token_unnamed_blockdata_ == NULL)
	ffesymbol_token_unnamed_blockdata_
	  = ffelex_token_new_name (FFETARGET_nameUNNAMED_BLOCK_DATA, wl, wc);
      t = ffesymbol_token_unnamed_blockdata_;
    }

  n = ffename_lookup (ffesymbol_local_, t);
  if (n != NULL)
    return ffename_symbol (n);	/* This will become an error. */

  n = ffename_find (ffesymbol_global_, t);
  s = ffename_symbol (n);
  if (s != NULL)
    {
      if (user)
	ffesymbol_check (s, t, FALSE);
      return s;
    }

  s = ffesymbol_new_ (n);
  if (user)
    ffesymbol_check (s, t, FALSE);

  /* A program unit name also is in the local name space. */

  n = ffename_find (ffesymbol_local_, t);
  ffename_set_symbol (n, s);
  s->other_space_name = n;

  ffeglobal_new_blockdata (s, t);	/* Detect conflicts, when
					   appropriate. */

  return s;
}

/* Declare a common block (named or unnamed).

   Retrieves or creates the ffesymbol for the specified common block (blank
   common if t is NULL).  Doesn't actually ensure the named item is a
   common block; the caller must handle that.  */

ffesymbol
ffesymbol_declare_cblock (ffelexToken t, ffewhereLine wl, ffewhereColumn wc)
{
  ffename n;
  ffesymbol s;
  bool blank;

  assert (!ffesymbol_retractable_);

  if (t == NULL)
    {
      blank = TRUE;
      if (ffesymbol_token_blank_common_ == NULL)
	ffesymbol_token_blank_common_
	  = ffelex_token_new_name (FFETARGET_nameBLANK_COMMON, wl, wc);
      t = ffesymbol_token_blank_common_;
    }
  else
    blank = FALSE;

  n = ffename_find (ffesymbol_global_, t);
  s = ffename_symbol (n);
  if (s != NULL)
    {
      if (!blank)
	ffesymbol_check (s, t, FALSE);
      return s;
    }

  s = ffesymbol_new_ (n);
  if (!blank)
    ffesymbol_check (s, t, FALSE);

  ffeglobal_new_common (s, t, blank);	/* Detect conflicts. */

  return s;
}

/* Declare a FUNCTION program unit (with distinct RESULT() name).

   Retrieves or creates the ffesymbol for the specified function.  Doesn't
   actually ensure the named item is a function; the caller must handle
   that.

   If FUNCTION with RESULT() is specified but the names are the same,
   pretend as though RESULT() was not specified, and don't call this
   function; use ffesymbol_declare_funcunit() instead.	*/

ffesymbol
ffesymbol_declare_funcnotresunit (ffelexToken t)
{
  ffename n;
  ffesymbol s;

  assert (t != NULL);
  assert (!ffesymbol_retractable_);

  n = ffename_lookup (ffesymbol_local_, t);
  if (n != NULL)
    return ffename_symbol (n);	/* This will become an error. */

  n = ffename_find (ffesymbol_global_, t);
  s = ffename_symbol (n);
  if (s != NULL)
    {
      ffesymbol_check (s, t, FALSE);
      return s;
    }

  s = ffesymbol_new_ (n);
  ffesymbol_check (s, t, FALSE);

  /* A FUNCTION program unit name also is in the local name space; handle it
     here since RESULT() is a different name and is handled separately. */

  n = ffename_find (ffesymbol_local_, t);
  ffename_set_symbol (n, s);
  s->other_space_name = n;

  ffeglobal_new_function (s, t);/* Detect conflicts, when appropriate. */

  return s;
}

/* Declare a function result.

   Retrieves or creates the ffesymbol for the specified function result,
   whether specified via a distinct RESULT() or by default in a FUNCTION or
   ENTRY statement.  */

ffesymbol
ffesymbol_declare_funcresult (ffelexToken t)
{
  ffename n;
  ffesymbol s;

  assert (t != NULL);
  assert (!ffesymbol_retractable_);

  n = ffename_find (ffesymbol_local_, t);
  s = ffename_symbol (n);
  if (s != NULL)
    return s;

  return ffesymbol_new_ (n);
}

/* Declare a FUNCTION program unit with no RESULT().

   Retrieves or creates the ffesymbol for the specified function.  Doesn't
   actually ensure the named item is a function; the caller must handle
   that.

   This is the function to call when the FUNCTION or ENTRY statement has
   no separate and distinct name specified via RESULT().  That's because
   this function enters the global name of the function in only the global
   name space.	ffesymbol_declare_funcresult() must still be called to
   declare the name for the function result in the local name space.  */

ffesymbol
ffesymbol_declare_funcunit (ffelexToken t)
{
  ffename n;
  ffesymbol s;

  assert (t != NULL);
  assert (!ffesymbol_retractable_);

  n = ffename_find (ffesymbol_global_, t);
  s = ffename_symbol (n);
  if (s != NULL)
    {
      ffesymbol_check (s, t, FALSE);
      return s;
    }

  s = ffesymbol_new_ (n);
  ffesymbol_check (s, t, FALSE);

  ffeglobal_new_function (s, t);/* Detect conflicts. */

  return s;
}

/* Declare a local entity.

   Retrieves or creates the ffesymbol for the specified local entity.
   Set maybe_intrin TRUE if this name might turn out to name an
   intrinsic (legitimately); otherwise if the name doesn't meet the
   requirements for a user-defined symbol name, a diagnostic will be
   issued right away rather than waiting until the intrinsicness of the
   symbol is determined.  */

ffesymbol
ffesymbol_declare_local (ffelexToken t, bool maybe_intrin)
{
  ffename n;
  ffesymbol s;

  assert (t != NULL);

  /* If we're parsing within a statement function definition, return the
     symbol if already known (a dummy argument for the statement function).
     Otherwise continue on, which means the symbol is declared within the
     containing (local) program unit rather than the statement function
     definition.  */

  if ((ffesymbol_sfunc_ != NULL)
      && ((n = ffename_lookup (ffesymbol_sfunc_, t)) != NULL))
    return ffename_symbol (n);

  n = ffename_find (ffesymbol_local_, t);
  s = ffename_symbol (n);
  if (s != NULL)
    {
      ffesymbol_check (s, t, maybe_intrin);
      return s;
    }

  s = ffesymbol_new_ (n);
  ffesymbol_check (s, t, maybe_intrin);
  return s;
}

/* Declare a main program unit.

   Retrieves or creates the ffesymbol for the specified main program unit
   (unnamed main program unit if t is NULL).  Doesn't actually ensure the
   named item is a program; the caller must handle that.  */

ffesymbol
ffesymbol_declare_programunit (ffelexToken t, ffewhereLine wl,
			       ffewhereColumn wc)
{
  ffename n;
  ffesymbol s;
  bool user = (t != NULL);

  assert (!ffesymbol_retractable_);

  if (t == NULL)
    {
      if (ffesymbol_token_unnamed_main_ == NULL)
	ffesymbol_token_unnamed_main_
	  = ffelex_token_new_name (FFETARGET_nameUNNAMED_MAIN, wl, wc);
      t = ffesymbol_token_unnamed_main_;
    }

  n = ffename_lookup (ffesymbol_local_, t);
  if (n != NULL)
    return ffename_symbol (n);	/* This will become an error. */

  n = ffename_find (ffesymbol_global_, t);
  s = ffename_symbol (n);
  if (s != NULL)
    {
      if (user)
	ffesymbol_check (s, t, FALSE);
      return s;
    }

  s = ffesymbol_new_ (n);
  if (user)
    ffesymbol_check (s, t, FALSE);

  /* A program unit name also is in the local name space. */

  n = ffename_find (ffesymbol_local_, t);
  ffename_set_symbol (n, s);
  s->other_space_name = n;

  ffeglobal_new_program (s, t);	/* Detect conflicts. */

  return s;
}

/* Declare a statement-function dummy.

   Retrieves or creates the ffesymbol for the specified statement
   function dummy.  Also ensures that it has a link to the parent (local)
   ffesymbol with the same name, creating it if necessary.  */

ffesymbol
ffesymbol_declare_sfdummy (ffelexToken t)
{
  ffename n;
  ffesymbol s;
  ffesymbol sp;			/* Parent symbol in local area. */

  assert (t != NULL);

  n = ffename_find (ffesymbol_local_, t);
  sp = ffename_symbol (n);
  if (sp == NULL)
    sp = ffesymbol_new_ (n);
  ffesymbol_check (sp, t, FALSE);

  n = ffename_find (ffesymbol_sfunc_, t);
  s = ffename_symbol (n);
  if (s == NULL)
    {
      s = ffesymbol_new_ (n);
      s->sfa_dummy_parent = sp;
    }
  else
    assert (s->sfa_dummy_parent == sp);

  return s;
}

/* Declare a subroutine program unit.

   Retrieves or creates the ffesymbol for the specified subroutine
   Doesn't actually ensure the named item is a subroutine; the caller must
   handle that.  */

ffesymbol
ffesymbol_declare_subrunit (ffelexToken t)
{
  ffename n;
  ffesymbol s;

  assert (!ffesymbol_retractable_);
  assert (t != NULL);

  n = ffename_lookup (ffesymbol_local_, t);
  if (n != NULL)
    return ffename_symbol (n);	/* This will become an error. */

  n = ffename_find (ffesymbol_global_, t);
  s = ffename_symbol (n);
  if (s != NULL)
    {
      ffesymbol_check (s, t, FALSE);
      return s;
    }

  s = ffesymbol_new_ (n);
  ffesymbol_check (s, t, FALSE);

  /* A program unit name also is in the local name space. */

  n = ffename_find (ffesymbol_local_, t);
  ffename_set_symbol (n, s);
  s->other_space_name = n;

  ffeglobal_new_subroutine (s, t);	/* Detect conflicts, when
					   appropriate. */

  return s;
}

/* Call given fn with all local/global symbols.

   ffesymbol (*fn) (ffesymbol s);
   ffesymbol_drive (fn);  */

void
ffesymbol_drive (ffesymbol (*fn) (ffesymbol))
{
  assert (ffesymbol_sfunc_ == NULL);	/* Might be ok, but not for current
					   uses. */
  ffename_space_drive_symbol (ffesymbol_local_, fn);
  ffename_space_drive_symbol (ffesymbol_global_, fn);
}

/* Call given fn with all sfunc-only symbols.

   ffesymbol (*fn) (ffesymbol s);
   ffesymbol_drive_sfnames (fn);  */

void
ffesymbol_drive_sfnames (ffesymbol (*fn) (ffesymbol))
{
  ffename_space_drive_symbol (ffesymbol_sfunc_, fn);
}

/* Dump info on the symbol for debugging purposes.  */

#if FFECOM_targetCURRENT == FFECOM_targetFFE
void
ffesymbol_dump (ffesymbol s)
{
  ffeinfoKind k;
  ffeinfoWhere w;

  assert (s != NULL);

  if (ffeinfo_size (s->info) != FFETARGET_charactersizeNONE)
    fprintf (dmpout, "%s:%d%s%s*%" ffetargetCharacterSize_f "u",
	     ffesymbol_text (s),
	     (int) ffeinfo_rank (s->info),
	     ffeinfo_basictype_string (ffeinfo_basictype (s->info)),
	     ffeinfo_kindtype_string (ffeinfo_kindtype (s->info)),
	     ffeinfo_size (s->info));
  else
    fprintf (dmpout, "%s:%d%s%s",
	     ffesymbol_text (s),
	     (int) ffeinfo_rank (s->info),
	     ffeinfo_basictype_string (ffeinfo_basictype (s->info)),
	     ffeinfo_kindtype_string (ffeinfo_kindtype (s->info)));
  if ((k = ffeinfo_kind (s->info)) != FFEINFO_kindNONE)
    fprintf (dmpout, "/%s", ffeinfo_kind_string (k));
  if ((w = ffeinfo_where (s->info)) != FFEINFO_whereNONE)
    fprintf (dmpout, "@%s", ffeinfo_where_string (w));

  if ((s->generic != FFEINTRIN_genNONE)
      || (s->specific != FFEINTRIN_specNONE)
      || (s->implementation != FFEINTRIN_impNONE))
    fprintf (dmpout, "{%s:%s:%s}",
	     ffeintrin_name_generic (s->generic),
	     ffeintrin_name_specific (s->specific),
	     ffeintrin_name_implementation (s->implementation));
}
#endif

/* Produce generic error message about a symbol.

   For now, just output error message using symbol's name and pointing to
   the token.  */

void
ffesymbol_error (ffesymbol s, ffelexToken t)
{
  if ((t != NULL)
      && ffest_ffebad_start (FFEBAD_SYMERR))
    {
      ffebad_string (ffesymbol_text (s));
      ffebad_here (0, ffelex_token_where_line (t),
		   ffelex_token_where_column (t));
      ffebad_here (1, ffesymbol_where_line (s), ffesymbol_where_column (s));
      ffebad_finish ();
    }

  if (ffesymbol_attr (s, FFESYMBOL_attrANY))
    return;

  ffesymbol_signal_change (s);	/* May need to back up to previous version. */
  if ((ffesymbol_attrs (s) & FFESYMBOL_attrsCBLOCK)
      || (ffesymbol_kind (s) == FFEINFO_kindNAMELIST))
    ffebld_end_list (ffesymbol_ptr_to_listbottom (s));
  ffesymbol_set_attr (s, FFESYMBOL_attrANY);
  ffesymbol_set_info (s, ffeinfo_new_any ());
  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
  if (s->check_state == FFESYMBOL_checkstatePENDING_)
    ffelex_token_kill (s->check_token);
  s->check_state = FFESYMBOL_checkstateCHECKED_;
  s = ffecom_sym_learned (s);
  ffesymbol_signal_unreported (s);
}

void
ffesymbol_init_0 ()
{
  ffesymbolAttrs attrs = FFESYMBOL_attrsetNONE;

  assert (FFESYMBOL_state == ARRAY_SIZE (ffesymbol_state_name_));
  assert (FFESYMBOL_attr == ARRAY_SIZE (ffesymbol_attr_name_));
  assert (attrs == FFESYMBOL_attrsetNONE);
  attrs = ((ffesymbolAttrs) 1 << FFESYMBOL_attr);
  assert (attrs != 0);
}

void
ffesymbol_init_1 ()
{
#if FFESYMBOL_globalCURRENT_ == FFESYMBOL_globalFILE_
  ffesymbol_global_ = ffename_space_new (ffe_pool_file ());
#endif
}

void
ffesymbol_init_2 ()
{
}

void
ffesymbol_init_3 ()
{
#if FFESYMBOL_globalCURRENT_ == FFESYMBOL_globalPROGUNIT_
  ffesymbol_global_ = ffename_space_new (ffe_pool_program_unit ());
#endif
  ffesymbol_local_ = ffename_space_new (ffe_pool_program_unit ());
}

void
ffesymbol_init_4 ()
{
  ffesymbol_sfunc_ = ffename_space_new (ffe_pool_program_unit ());
}

/* Look up a local entity.

   Retrieves the ffesymbol for the specified local entity, or returns NULL
   if no local entity by that name exists.  */

ffesymbol
ffesymbol_lookup_local (ffelexToken t)
{
  ffename n;
  ffesymbol s;

  assert (t != NULL);

  n = ffename_lookup (ffesymbol_local_, t);
  if (n == NULL)
    return NULL;

  s = ffename_symbol (n);
  return s;			/* May be NULL here, too. */
}

/* Registers the symbol as one that is referenced by the
   current program unit.  Currently applies only to
   symbols known to have global interest (globals and
   intrinsics).

   s is the (global/intrinsic) symbol referenced; t is the
   referencing token; explicit is TRUE if the reference
   is, e.g., INTRINSIC FOO.  */

void
ffesymbol_reference (ffesymbol s, ffelexToken t, bool explicit)
{
  ffename gn;
  ffesymbol gs = NULL;
  ffeinfoKind kind;
  ffeinfoWhere where;
  bool okay;

  if (ffesymbol_retractable_)
    return;

  if (t == NULL)
    t = ffename_token (s->name);	/* Use the first reference in this program unit. */

  kind = ffesymbol_kind (s);
  where = ffesymbol_where (s);

  if (where == FFEINFO_whereINTRINSIC)
    {
      ffeglobal_ref_intrinsic (s, t,
			       explicit
			       || s->explicit_where
			       || ffeintrin_is_standard (s->generic, s->specific));
      return;
    }

  if ((where != FFEINFO_whereGLOBAL)
      && ((where != FFEINFO_whereLOCAL)
	  || ((kind != FFEINFO_kindFUNCTION)
	      && (kind != FFEINFO_kindSUBROUTINE))))
    return;

  gn = ffename_lookup (ffesymbol_global_, t);
  if (gn != NULL)
    gs = ffename_symbol (gn);
  if ((gs != NULL) && (gs != s))
    {
      /* We have just discovered another global symbol with the same name
	 but a different `nature'.  Complain.  Note that COMMON /FOO/ can
	 coexist with local symbol FOO, e.g. local variable, just not with
	 CALL FOO, hence the separate namespaces.  */

      ffesymbol_error (gs, t);
      ffesymbol_error (s, NULL);
      return;
    }

  switch (kind)
    {
    case FFEINFO_kindBLOCKDATA:
      okay = ffeglobal_ref_blockdata (s, t);
      break;

    case FFEINFO_kindSUBROUTINE:
      okay = ffeglobal_ref_subroutine (s, t);
      break;

    case FFEINFO_kindFUNCTION:
      okay = ffeglobal_ref_function (s, t);
      break;

    case FFEINFO_kindNONE:
      okay = ffeglobal_ref_external (s, t);
      break;

    default:
      assert ("bad kind in global ref" == NULL);
      return;
    }

  if (! okay)
    ffesymbol_error (s, NULL);
}

/* Report info on the symbol for debugging purposes.  */

#if FFECOM_targetCURRENT == FFECOM_targetFFE
ffesymbol
ffesymbol_report (ffesymbol s)
{
  ffeinfoKind k;
  ffeinfoWhere w;

  assert (s != NULL);

  if (s->reported)
    return s;

  s->reported = TRUE;

  if (ffeinfo_size (s->info) != FFETARGET_charactersizeNONE)
    fprintf (dmpout, "\"%s\": %s %s %d%s%s*%" ffetargetCharacterSize_f "u",
	     ffesymbol_text (s),
	     ffesymbol_state_string (s->state),
	     ffesymbol_attrs_string (s->attrs),
	     (int) ffeinfo_rank (s->info),
	     ffeinfo_basictype_string (ffeinfo_basictype (s->info)),
	     ffeinfo_kindtype_string (ffeinfo_kindtype (s->info)),
	     ffeinfo_size (s->info));
  else
    fprintf (dmpout, "\"%s\": %s %s %d%s%s",
	     ffesymbol_text (s),
	     ffesymbol_state_string (s->state),
	     ffesymbol_attrs_string (s->attrs),
	     (int) ffeinfo_rank (s->info),
	     ffeinfo_basictype_string (ffeinfo_basictype (s->info)),
	     ffeinfo_kindtype_string (ffeinfo_kindtype (s->info)));
  if ((k = ffeinfo_kind (s->info)) != FFEINFO_kindNONE)
    fprintf (dmpout, "/%s", ffeinfo_kind_string (k));
  if ((w = ffeinfo_where (s->info)) != FFEINFO_whereNONE)
    fprintf (dmpout, "@%s", ffeinfo_where_string (w));
  fputc ('\n', dmpout);

  if (s->dims != NULL)
    {
      fprintf (dmpout, "  dims: ");
      ffebld_dump (s->dims);
      fputs ("\n", dmpout);
    }

  if (s->extents != NULL)
    {
      fprintf (dmpout, "  extents: ");
      ffebld_dump (s->extents);
      fputs ("\n", dmpout);
    }

  if (s->dim_syms != NULL)
    {
      fprintf (dmpout, "  dim syms: ");
      ffebld_dump (s->dim_syms);
      fputs ("\n", dmpout);
    }

  if (s->array_size != NULL)
    {
      fprintf (dmpout, "  array size: ");
      ffebld_dump (s->array_size);
      fputs ("\n", dmpout);
    }

  if (s->init != NULL)
    {
      fprintf (dmpout, "  init-value: ");
      if (ffebld_op (s->init) == FFEBLD_opANY)
	fputs ("<any>\n", dmpout);
      else
	{
	  ffebld_dump (s->init);
	  fputs ("\n", dmpout);
	}
    }

  if (s->accretion != NULL)
    {
      fprintf (dmpout, "  accretion (%" ffetargetOffset_f "d left): ",
	       s->accretes);
      ffebld_dump (s->accretion);
      fputs ("\n", dmpout);
    }
  else if (s->accretes != 0)
    fprintf (dmpout, "  accretes!! = %" ffetargetOffset_f "d left\n",
	     s->accretes);

  if (s->dummy_args != NULL)
    {
      fprintf (dmpout, "  dummies: ");
      ffebld_dump (s->dummy_args);
      fputs ("\n", dmpout);
    }

  if (s->namelist != NULL)
    {
      fprintf (dmpout, "  namelist: ");
      ffebld_dump (s->namelist);
      fputs ("\n", dmpout);
    }

  if (s->common_list != NULL)
    {
      fprintf (dmpout, "  common-list: ");
      ffebld_dump (s->common_list);
      fputs ("\n", dmpout);
    }

  if (s->sfunc_expr != NULL)
    {
      fprintf (dmpout, "  sfunc expression: ");
      ffebld_dump (s->sfunc_expr);
      fputs ("\n", dmpout);
    }

  if (s->is_save)
    {
      fprintf (dmpout, "  SAVEd\n");
    }

  if (s->is_init)
    {
      fprintf (dmpout, "  initialized\n");
    }

  if (s->do_iter)
    {
      fprintf (dmpout, "  DO-loop iteration variable (currently)\n");
    }

  if (s->explicit_where)
    {
      fprintf (dmpout, "  Explicit INTRINSIC/EXTERNAL\n");
    }

  if (s->namelisted)
    {
      fprintf (dmpout, "  Namelisted\n");
    }

  if (s->common != NULL)
    {
      fprintf (dmpout, "  COMMON area: %s\n", ffesymbol_text (s->common));
    }

  if (s->equiv != NULL)
    {
      fprintf (dmpout, "  EQUIVALENCE information: ");
      ffeequiv_dump (s->equiv);
      fputs ("\n", dmpout);
    }

  if (s->storage != NULL)
    {
      fprintf (dmpout, "  Storage: ");
      ffestorag_dump (s->storage);
      fputs ("\n", dmpout);
    }

  return s;
}
#endif

/* Report info on the symbols.	*/

#if FFECOM_targetCURRENT == FFECOM_targetFFE
void
ffesymbol_report_all ()
{
  ffename_space_drive_symbol (ffesymbol_sfunc_, ffesymbol_report);
  ffename_space_drive_symbol (ffesymbol_local_, ffesymbol_report);
  ffename_space_drive_symbol (ffesymbol_global_, ffesymbol_report);
}
#endif

/* Resolve symbol that has become known intrinsic or non-intrinsic.  */

void
ffesymbol_resolve_intrin (ffesymbol s)
{
  char c;
  ffebad bad;

  if (!ffesrc_check_symbol ())
    return;
  if (s->check_state != FFESYMBOL_checkstatePENDING_)
    return;
  if (ffebad_inhibit ())
    return;			/* We'll get back to this later. */

  if (ffesymbol_where (s) != FFEINFO_whereINTRINSIC)
    {
      bad = ffesymbol_check_token_ (s->check_token, &c);
      assert (bad != FFEBAD);	/* How did this suddenly become ok? */
      ffesymbol_whine_state_ (bad, s->check_token, c);
    }

  s->check_state = FFESYMBOL_checkstateCHECKED_;
  ffelex_token_kill (s->check_token);
}

/* Retract or cancel retract list.  */

void
ffesymbol_retract (bool retract)
{
  ffesymbolRetract_ r;
  ffename name;
  ffename other_space_name;
  ffesymbol ls;
  ffesymbol os;

  assert (ffesymbol_retractable_);

  ffesymbol_retractable_ = FALSE;

  for (r = ffesymbol_retract_first_; r != NULL; r = r->next)
    {
      ls = r->live;
      os = r->symbol;
      switch (r->command)
	{
	case FFESYMBOL_retractcommandDELETE_:
	  if (retract)
	    {
	      ffecom_sym_retract (ls);
	      name = ls->name;
	      other_space_name = ls->other_space_name;
	      ffesymbol_unhook_ (ls);
	      malloc_kill_ks (FFESYMBOL_SPACE_POOL_, ls, sizeof (*ls));
	      if (name != NULL)
		ffename_set_symbol (name, NULL);
	      if (other_space_name != NULL)
		ffename_set_symbol (other_space_name, NULL);
	    }
	  else
	    {
	      ffecom_sym_commit (ls);
	      ls->have_old = FALSE;
	    }
	  break;

	case FFESYMBOL_retractcommandRETRACT_:
	  if (retract)
	    {
	      ffecom_sym_retract (ls);
	      ffesymbol_unhook_ (ls);
	      *ls = *os;
	      malloc_kill_ks (FFESYMBOL_SPACE_POOL_, os, sizeof (*os));
	    }
	  else
	    {
	      ffecom_sym_commit (ls);
	      ffesymbol_unhook_ (os);
	      malloc_kill_ks (FFESYMBOL_SPACE_POOL_, os, sizeof (*os));
	      ls->have_old = FALSE;
	    }
	  break;

	default:
	  assert ("bad command" == NULL);
	  break;
	}
    }
}

/* Return retractable flag.  */

bool
ffesymbol_retractable ()
{
  return ffesymbol_retractable_;
}

/* Set retractable flag, retract pool.

   Between this call and ffesymbol_retract, any changes made to existing
   symbols cause the previous versions of those symbols to be saved, and any
   newly created symbols to have their previous nonexistence saved.  When
   ffesymbol_retract is called, this information either is used to retract
   the changes and new symbols, or is discarded.  */

void
ffesymbol_set_retractable (mallocPool pool)
{
  assert (!ffesymbol_retractable_);

  ffesymbol_retractable_ = TRUE;
  ffesymbol_retract_pool_ = pool;
  ffesymbol_retract_list_ = &ffesymbol_retract_first_;
  ffesymbol_retract_first_ = NULL;
}

/* Existing symbol about to be changed; save?

   Call this function before changing a symbol if it is possible that
   the current actions may need to be undone (i.e. one of several possible
   statement forms are being used to analyze the current system).

   If the "retractable" flag is not set, just return.
   Else, if the symbol's "have_old" flag is set, just return.
   Else, make a copy of the symbol and add it to the "retract" list, set
   the "have_old" flag, and return.  */

void
ffesymbol_signal_change (ffesymbol s)
{
  ffesymbolRetract_ r;
  ffesymbol sym;

  if (!ffesymbol_retractable_ || s->have_old)
    return;

  r = (ffesymbolRetract_) malloc_new_kp (ffesymbol_retract_pool_,
					 "FFESYMBOL retract", sizeof (*r));
  r->next = NULL;
  r->command = FFESYMBOL_retractcommandRETRACT_;
  r->live = s;
  r->symbol = sym = (ffesymbol) malloc_new_ks (FFESYMBOL_SPACE_POOL_,
					       "FFESYMBOL", sizeof (*sym));
  *sym = *s;			/* Make an exact copy of the symbol in case
				   we need it back. */
  sym->info = ffeinfo_use (s->info);
  if (s->check_state == FFESYMBOL_checkstatePENDING_)
    sym->check_token = ffelex_token_use (s->check_token);

  *ffesymbol_retract_list_ = r;
  ffesymbol_retract_list_ = &r->next;

  s->have_old = TRUE;
}

/* Returns the string based on the state.  */

const char *
ffesymbol_state_string (ffesymbolState state)
{
  if (state >= ARRAY_SIZE (ffesymbol_state_name_))
    return "?\?\?";
  return ffesymbol_state_name_[state];
}

void
ffesymbol_terminate_0 ()
{
}

void
ffesymbol_terminate_1 ()
{
#if FFESYMBOL_globalCURRENT_ == FFESYMBOL_globalFILE_
  ffename_space_drive_symbol (ffesymbol_global_, ffesymbol_unhook_);
  ffename_space_kill (ffesymbol_global_);
  ffesymbol_global_ = NULL;

  ffesymbol_kill_manifest_ ();
#endif
}

void
ffesymbol_terminate_2 ()
{
#if FFESYMBOL_globalCURRENT_ == FFESYMBOL_globalPROGUNIT_
  ffesymbol_kill_manifest_ ();
#endif
}

void
ffesymbol_terminate_3 ()
{
#if FFESYMBOL_globalCURRENT_ == FFESYMBOL_globalPROGUNIT_
  ffename_space_drive_symbol (ffesymbol_global_, ffesymbol_unhook_);
  ffename_space_kill (ffesymbol_global_);
#endif
  ffename_space_drive_symbol (ffesymbol_local_, ffesymbol_unhook_);
  ffename_space_kill (ffesymbol_local_);
#if FFESYMBOL_globalCURRENT_ == FFESYMBOL_globalPROGUNIT_
  ffesymbol_global_ = NULL;
#endif
  ffesymbol_local_ = NULL;
}

void
ffesymbol_terminate_4 ()
{
  ffename_space_drive_symbol (ffesymbol_sfunc_, ffesymbol_unhook_);
  ffename_space_kill (ffesymbol_sfunc_);
  ffesymbol_sfunc_ = NULL;
}

/* Update INIT info to TRUE and all equiv/storage too.

   If INIT flag is TRUE, does nothing.	Else sets it to TRUE and calls
   on the ffeequiv and ffestorag modules to update their INIT flags if
   the <s> symbol has those objects, and also updates the common area if
   it exists.  */

void
ffesymbol_update_init (ffesymbol s)
{
  ffebld item;

  if (s->is_init)
    return;

  s->is_init = TRUE;

  if ((s->equiv != NULL)
      && !ffeequiv_is_init (s->equiv))
    ffeequiv_update_init (s->equiv);

  if ((s->storage != NULL)
      && !ffestorag_is_init (s->storage))
    ffestorag_update_init (s->storage);

  if ((s->common != NULL)
      && (!ffesymbol_is_init (s->common)))
    ffesymbol_update_init (s->common);

  for (item = s->common_list; item != NULL; item = ffebld_trail (item))
    {
      if (!ffesymbol_is_init (ffebld_symter (ffebld_head (item))))
	ffesymbol_update_init (ffebld_symter (ffebld_head (item)));
    }
}

/* Update SAVE info to TRUE and all equiv/storage too.

   If SAVE flag is TRUE, does nothing.	Else sets it to TRUE and calls
   on the ffeequiv and ffestorag modules to update their SAVE flags if
   the <s> symbol has those objects, and also updates the common area if
   it exists.  */

void
ffesymbol_update_save (ffesymbol s)
{
  ffebld item;

  if (s->is_save)
    return;

  s->is_save = TRUE;

  if ((s->equiv != NULL)
      && !ffeequiv_is_save (s->equiv))
    ffeequiv_update_save (s->equiv);

  if ((s->storage != NULL)
      && !ffestorag_is_save (s->storage))
    ffestorag_update_save (s->storage);

  if ((s->common != NULL)
      && (!ffesymbol_is_save (s->common)))
    ffesymbol_update_save (s->common);

  for (item = s->common_list; item != NULL; item = ffebld_trail (item))
    {
      if (!ffesymbol_is_save (ffebld_symter (ffebld_head (item))))
	ffesymbol_update_save (ffebld_symter (ffebld_head (item)));
    }
}
