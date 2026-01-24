/* Extract tags from phrases.
   Copyright (C) 2001-2023 J. Marcel van der Veer.
   Copyright (C) 2025 Jose E. Marchesi.

   Original implementation by J. Marcel van der Veer.
   Adapted for GCC by Jose E. Marchesi.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "a68.h"

/* This is part of the bottom-up parser.  Here is a set of routines that gather
  definitions from phrases.  This way we can apply tags before defining them.
  These routines do not look very elegant as they have to scan through all kind
  of symbols to find a pattern that they recognise.  */

/* Insert alt equals symbol.  */

static void
insert_alt_equals (NODE_T *p)
{
  NODE_T *q = a68_new_node ();
  *q = *p;
  INFO (q) = a68_new_node_info ();
  *INFO (q) = *INFO (p);
  GINFO (q) = a68_new_genie_info ();
  *GINFO (q) = *GINFO (p);
  ATTRIBUTE (q) = ALT_EQUALS_SYMBOL;
  NSYMBOL (q) = TEXT (a68_add_token (&A68 (top_token), "="));
  NEXT (p) = q;
  PREVIOUS (q) = p;
  if (NEXT (q) != NO_NODE)
    PREVIOUS (NEXT (q)) = q;
}

/* Detect redefined keyword.  */

static void
detect_redefined_keyword (NODE_T *p, int construct)
{
  if (p != NO_NODE && a68_whether (p, KEYWORD, EQUALS_SYMBOL, STOP))
    a68_error (p, "attempt to redefine keyword Y in A",
	       NSYMBOL (p), construct);
}

/* Skip anything until a FED or ALT_ACCESS_SYMBOL is found.  */

static NODE_T *
skip_module_text (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, FED_SYMBOL) || IS (p, ALT_ACCESS_SYMBOL))
	return p;
    }

  return NO_NODE;
}

/* Skip anything until a comma, semicolon or EXIT is found.  */

static NODE_T *
skip_unit (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, COMMA_SYMBOL))
	return p;
      else if (IS (p, SEMI_SYMBOL))
	return p;
      else if (IS (p, EXIT_SYMBOL))
	return p;
    }
  return NO_NODE;
}

/* Attribute of entry in symbol table.  */

static int
find_tag_definition (TABLE_T *table, const char *name)
{
  if (table != NO_TABLE)
    {
      int ret = 0;
      bool found = false;
      for (TAG_T *s = INDICANTS (table); s != NO_TAG && !found; FORWARD (s))
	{
	  if (NSYMBOL (NODE (s)) == name || strcmp (NSYMBOL (NODE (s)), name) == 0)
	    {
	      ret += INDICANT;
	      found = true;
	    }
	}
      found = false;
      for (TAG_T *s = OPERATORS (table); s != NO_TAG && !found; FORWARD (s))
	{
	  if (NSYMBOL (NODE (s)) == name || strcmp (NSYMBOL (NODE (s)), name) == 0)
	    {
	      ret += OPERATOR;
	      found = true;
	    }
	}
      found = false;
      for (TAG_T *s = MODULES (table); s != NO_TAG && !found; FORWARD (s))
	{
	  if (NSYMBOL (NODE (s)) == name || strcmp (NSYMBOL (NODE (s)), name) == 0)
	    {
	      ret += MODULE_INDICANT;
	      found = true;
	    }
	}

      if (ret == 0)
	return find_tag_definition (PREVIOUS (table), name);
      else
	return ret;
    }
  else
    return 0;
}

/* Fill in whether bold tag is operator, indicant, module indicant or language
   indicant.  */

void
a68_elaborate_bold_tags (NODE_T *p)
{
  for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
    {
      if (IS (q, BOLD_TAG))
	{
	  if (PREVIOUS (q) != NO_NODE
	      && IS (PREVIOUS (q), FORMAL_NEST_SYMBOL))
	    {
	      if (strcmp (NSYMBOL (q), "C") != 0)
		a68_error (q, "S is not a valid language indication");
	      else
		ATTRIBUTE (q) = LANGUAGE_INDICANT;
	    }
	  else
	    {
	      switch (find_tag_definition (TABLE (q), NSYMBOL (q)))
		{
		case 0:
		  a68_error (q, "tag S has not been declared properly");
		  break;
		case INDICANT:
		  ATTRIBUTE (q) = INDICANT;
		  break;
		case OPERATOR:
		  ATTRIBUTE (q) = OPERATOR;
		  break;
		case MODULE_INDICANT:
		  ATTRIBUTE (q) = MODULE_INDICANT;
		  break;
		}
	    }
	}
    }
}

/* Skip declarer, or argument pack and declarer.  */

static NODE_T *
skip_pack_declarer (NODE_T *p)
{
  /* Skip PUB () REF [] REF FLEX [] [] ...  */
  while (p != NO_NODE
	 && (a68_is_one_of (p, SUB_SYMBOL, OPEN_SYMBOL, REF_SYMBOL,
			    FLEX_SYMBOL, SHORT_SYMBOL, LONG_SYMBOL, STOP)))
    {
      FORWARD (p);
    }

  /* Skip STRUCT (), UNION () or PROC [()].  */
  if (p != NO_NODE && (a68_is_one_of (p, STRUCT_SYMBOL, UNION_SYMBOL, STOP)))
    return NEXT (p);
  else if (p != NO_NODE && IS (p, PROC_SYMBOL))
    return skip_pack_declarer (NEXT (p));
  else
    return p;
}

/* Extract the revelation associated with the module MODULE.  The node Q is
   used for symbol table and diagnostic purposes.  Publicized modules are
   recursively extracted as well.  This call may result in one or more
   errors.  */

static void
extract_revelation (NODE_T *q, const char *module, TAG_T *tag)
{
  /* Import the MOIF and install it in the tag.  */
  MOIF_T *moif = a68_open_packet (module);
  if (moif == NULL)
    {
      a68_error (q, "cannot find module Z", module);
      return;
    }

  if (tag != NO_TAG)
    MOIF (tag) = moif;

  /* Store all the modes from the MOIF in the moid list.

     The front-end depends on being able to compare any two modes by pointer
     value.  For example, the parser mode equivalence and coercion code relies
     on this.  The lowerer also relies on this to make sure the same lowered
     trees are used for the same modes.  */

  for (MOID_T *m : MODES (moif))
    {
      /* Note that m == NO_MOID if the imported mode was already known to the
	 compiler and has been replaced.  */
      if (m != NO_MOID)
	{
	  MOID_T *r = a68_register_extra_mode (&TOP_MOID (&A68_JOB), m);
	  if (r != m)
	    {
	      printf ("r: %s\n", a68_moid_to_string (r, 80, NO_NODE, false));
	      printf ("m: %s\n", a68_moid_to_string (m, 80, NO_NODE, false));
	      gcc_unreachable ();
	    }
	}
    }

  /* Second thing to do is to extract the revelations of publicized modules in
     this moif.  This leads to recursive calls of this function.  Note that
     this should be done _after_ the modes get added to the global list of
     modes so mode deduplication in a68_open_packet in the recursive
     extract_revelation calls is properly done.  */

  for (EXTRACT_T *e : MODULES (moif))
    extract_revelation (q, EXTRACT_SYMBOL (e), NO_TAG);

  /* Store mode indicants from the MOIF in the symbol table,
     and also in the moid list.  */
  for (EXTRACT_T *e : INDICANTS (moif))
    {
      /* Indicants stored in the indicants area of a symbol
	 table are expected to be INDICANT nodes originating in
	 parsing, with the following subtree:

	 INDICANT - EQUALS_SYMBOL - DECLARER

	 where MOID (DECLARER) is determined to be the mode
	 associated by INDICANT, or its "equivalent" mode.
	 Therefore we have to synthesize something like that
	 here, since the mode comes from a module interface and
	 we don't have a declarer tree for it..  */

      /* INDICANT node.  */
      NODE_T *n = a68_some_node (a68_demangle_symbol (NAME (moif),
						      EXTRACT_SYMBOL (e)));
      /* EQUALS_SYMBOL node.  */
      NEXT (n) = a68_some_node ("=");
      ATTRIBUTE (NEXT (n)) = EQUALS_SYMBOL;
      /* DECLARER node.  */
      NEXT (NEXT (n)) = a68_some_node ("");
      LINE (INFO (n)) = LINE (INFO (q));
      NCHAR_IN_LINE (n) = STRING (LINE (INFO (n)));
      MOID (NEXT (NEXT (n))) = EXTRACT_MODE (e);
      TABLE (n) = TABLE (q);

      /* Now add the INDICANT subtree to the symbol table of
	 this access clause lexical level. */
      TAG_T *tag = a68_add_tag (TABLE (q), INDICANT, n, NO_MOID, 0);
      gcc_assert (tag != NO_TAG);

      /* Finally add the mode indicant to the moids list.  Note
	 that the mode in DECLARER is associated to the
	 INDICANT in the table at some point later, via
	 EQUIVALENT.  Hence the NO_MOID in the call to
	 a68_add_mode.  */
      MOID_T *effective_mode = a68_add_mode (&TOP_MOID (&A68_JOB), INDICANT,
					     0, n, NO_MOID, NO_PACK);
      gcc_assert (effective_mode != NO_MOID);
    }

  /* Store priorities from the MOIF.  */
  for (EXTRACT_T *e : PRIOS (moif))
    {
      NODE_T *n
	= a68_some_node (a68_demangle_symbol (NAME (moif),
					      EXTRACT_SYMBOL (e)));
      /* XXX sensible location somehow? */
      LINE (INFO (n)) = LINE (INFO (q));
      NCHAR_IN_LINE (n) = STRING (LINE (INFO (n)));
      TABLE (n) = TABLE (q);
      TAG_T *tag = a68_add_tag (TABLE (q), PRIO_SYMBOL, n, NO_MOID,
				EXTRACT_PRIO (e));
      gcc_assert (tag != NO_TAG);
      MOIF (tag) = moif; // XXX add to existing list of moifs.
    }

  /* Store identifiers from the MOIF.  */
  for (EXTRACT_T *e : IDENTIFIERS (moif))
    {
      NODE_T *n
	= a68_some_node (a68_demangle_symbol (NAME (moif),
					      EXTRACT_SYMBOL (e)));
      /* XXX sensible location somehow? */
      LINE (INFO (n)) = LINE (INFO (q));
      NCHAR_IN_LINE (n) = STRING (LINE (INFO (n)));
      TABLE (n) = TABLE (q);

      TAG_T *tag = a68_add_tag (TABLE (q), IDENTIFIER,
				n, EXTRACT_MODE (e), NORMAL_IDENTIFIER);
      gcc_assert (tag != NO_TAG);
      EXTERN_SYMBOL (tag) = ggc_strdup (EXTRACT_SYMBOL (e));
      VARIABLE (tag) = VARIABLE (e);
      IN_PROC (tag) = IN_PROC (e);
      HEAP (tag) = STATIC_SYMBOL;
      MOIF (tag) = moif;
    }

  /* Store operators from the MOIF.  */
  for (EXTRACT_T *e : OPERATORS (moif))
    {
      NODE_T *n
	= a68_some_node (a68_demangle_symbol (NAME (moif),
					      EXTRACT_SYMBOL (e),
					      true /* operator */));
      MOID (n) = EXTRACT_MODE (e);
      LINE (INFO (n)) = LINE (INFO (q));
      NCHAR_IN_LINE (n) = STRING (LINE (INFO (n)));
      TABLE (n) = TABLE (q);
      TAG_T *tag = a68_add_tag (TABLE (q), OP_SYMBOL,
				n, EXTRACT_MODE (e), STOP);
      gcc_assert (tag != NO_TAG);
      VARIABLE (tag) = VARIABLE (e);
      IN_PROC (tag) = IN_PROC (e);
      HEAP (tag) = STATIC_SYMBOL;
      MOIF (tag) = moif;
      EXTERN_SYMBOL (tag) = ggc_strdup (EXTRACT_SYMBOL (e));
    }
}

/* Search [MODE|MODULE] A = .., B = ..
   and    ACCESS A, B, ..
   and store indicants.  */

void
a68_extract_indicants (NODE_T *p)
{
  NODE_T *q = p;

  while (q != NO_NODE)
    {
      if (IS (q, ACCESS_SYMBOL) || IS (q, ALT_ACCESS_SYMBOL))
	{
	  /* An access clause implies the declaration of module indicants,
	     provided they are found in a suitable packet.  */
	  do
	    {
	      FORWARD (q);
	      if (q != NO_NODE)
		{
		  NODE_T *bold_tag = NO_NODE;

		  if (IS (q, BOLD_TAG))
		    {
		      bold_tag = q;
		      FORWARD (q);
		    }
		  else if (a68_whether (q, PUBLIC_SYMBOL, BOLD_TAG, STOP))
		    {
		      bold_tag = NEXT (q);
		      FORWARD (q);
		      FORWARD (q);
		    }

		  if (bold_tag != NO_NODE)
		    {
		      TAG_T *tag = a68_add_tag (TABLE (bold_tag), MODULE_SYMBOL, bold_tag, NO_MOID, STOP);
		      gcc_assert (tag != NO_TAG);
		      extract_revelation (bold_tag, NSYMBOL (bold_tag), tag);
		    }
		}
	    }
	  while (q != NO_NODE && IS (q, COMMA_SYMBOL));
	}
      else if (IS (q, MODULE_SYMBOL))
	{
	  bool siga = true;
	  do
	    {
	      FORWARD (q);
	      detect_redefined_keyword (q, MODULE_DECLARATION);
	      if (a68_whether (q, BOLD_TAG, EQUALS_SYMBOL, STOP))
		{
		  ATTRIBUTE (q) = DEFINING_MODULE_INDICANT;
		  FORWARD (q);
		  ATTRIBUTE (q) = EQUALS_SYMBOL; /* XXX why not ALT_EQUALS_SYMBOL */
		  if (NEXT (q) != NO_NODE && IS (NEXT (q), ACCESS_SYMBOL))
		    {
		      a68_error (NEXT (q),
				 "nested access clauses not allowed in module texts");
		      siga = false;
		    }
		  else
		    {
		      q = skip_module_text (NEXT (q));
		      FORWARD (q);
		    }
		}
	      else
		siga = false;
	    }
	  while (siga && q != NO_NODE && IS (q, COMMA_SYMBOL));
	}
      else if (IS (q, MODE_SYMBOL) || a68_whether (q, PUBLIC_SYMBOL, MODE_SYMBOL, STOP))
	{
	  bool siga = true;
	  bool is_public = false;

	  if (IS (q, PUBLIC_SYMBOL))
	    {
	      is_public = true;
	      FORWARD (q);
	    }

	  do
	    {
	      FORWARD (q);
	      detect_redefined_keyword (q, MODE_DECLARATION);
	      if (a68_whether (q, BOLD_TAG, EQUALS_SYMBOL, STOP))
		{
		  /* Store in the symbol table, but also in the moid list.
		     Position of definition (q) connects to this lexical
		     level!  */
		  if (a68_add_tag (TABLE (p), INDICANT, q, NO_MOID, STOP) == NO_TAG)
		    gcc_unreachable ();
		  if (a68_add_mode (&TOP_MOID (&A68_JOB), INDICANT, 0, q, NO_MOID, NO_PACK) == NO_MOID)
		    gcc_unreachable ();
		  ATTRIBUTE (q) = DEFINING_INDICANT;
		  PUBLICIZED (q) = is_public;
		  FORWARD (q);
		  ATTRIBUTE (q) = ALT_EQUALS_SYMBOL;
		  q = skip_pack_declarer (NEXT (q));
		  FORWARD (q);
		}
	      else
		siga = false;
	    }
	  while (siga && q != NO_NODE && IS (q, COMMA_SYMBOL));
	}
      else
	FORWARD (q);
    }
}

#define GET_PRIORITY(q, k)						\
  do									\
    {									\
      errno=0;								\
      (k) = atoi (NSYMBOL (q));						\
      if (errno != 0) {							\
	a68_error ((q), "invalid priority declaration");		\
	(k) = MAX_PRIORITY;						\
      } else if ((k) < 1 || (k) > MAX_PRIORITY) {			\
	a68_error ((q), "invalid priority declaration");		\
	(k) = MAX_PRIORITY;						\
      }									\
    }									\
  while (0)

/* Search PRIO X = .., Y = .. and store priorities.  */

void
a68_extract_priorities (NODE_T *p)
{
  NODE_T *q = p;
  while (q != NO_NODE)
    {
      if (IS (q, PRIO_SYMBOL) || a68_whether (q, PUBLIC_SYMBOL, PRIO_SYMBOL, STOP))
	{
	  bool siga = true;
	  bool is_public = false;

	  if (IS (q, PUBLIC_SYMBOL))
	    {
	      is_public = true;
	      FORWARD (q);
	    }

	  do
	    {
	      FORWARD (q);
	      detect_redefined_keyword (q, PRIORITY_DECLARATION);
	      /* An operator tag like ++ or && gives strange errors so we catch
		 it here.  */
	      if (a68_whether (q, OPERATOR, OPERATOR, STOP))
		{
		  NODE_T *y = q;
		  a68_error (q, "invalid operator tag");
		  ATTRIBUTE (q) = DEFINING_OPERATOR;
		  PUBLICIZED (q) = is_public;
		  /* Remove one superfluous operator, and hope it was only
		     one.  */
		  NEXT (q) = NEXT_NEXT (q);
		  PREVIOUS (NEXT (q)) = q;
		  FORWARD (q);
		  ATTRIBUTE (q) = ALT_EQUALS_SYMBOL;
		  FORWARD (q);
		  int k;
		  GET_PRIORITY (q, k);
		  ATTRIBUTE (q) = PRIORITY;
		  if (a68_add_tag (TABLE (p), PRIO_SYMBOL, y, NO_MOID, k) == NO_TAG)
		    gcc_unreachable ();
		  FORWARD (q);
		}
	      else if (a68_whether (q, OPERATOR, EQUALS_SYMBOL, INT_DENOTATION, STOP)
		       || a68_whether (q, EQUALS_SYMBOL, EQUALS_SYMBOL, INT_DENOTATION, STOP))
		{
		  NODE_T *y = q;
		  ATTRIBUTE (q) = DEFINING_OPERATOR;
		  PUBLICIZED (q) = is_public;
		  FORWARD (q);
		  ATTRIBUTE (q) = ALT_EQUALS_SYMBOL;
		  FORWARD (q);
		  int k;
		  GET_PRIORITY (q, k);
		  ATTRIBUTE (q) = PRIORITY;
		  if (a68_add_tag (TABLE (p), PRIO_SYMBOL, y, NO_MOID, k) == NO_TAG)
		    gcc_unreachable ();
		  FORWARD (q);
		}
	      else if (a68_whether (q, BOLD_TAG, IDENTIFIER, STOP))
		{
		  siga = false;
		}
	      else if (a68_whether (q, BOLD_TAG, EQUALS_SYMBOL, INT_DENOTATION, STOP))
		{
		  NODE_T *y = q;
		  ATTRIBUTE (q) = DEFINING_OPERATOR;
		  PUBLICIZED (q) = is_public;
		  FORWARD (q);
		  ATTRIBUTE (q) = ALT_EQUALS_SYMBOL;
		  FORWARD (q);
		  int k;
		  GET_PRIORITY (q, k);
		  ATTRIBUTE (q) = PRIORITY;
		  if (a68_add_tag (TABLE (p), PRIO_SYMBOL, y, NO_MOID, k) == NO_TAG)
		    gcc_unreachable ();
		  FORWARD (q);
		} else if (a68_whether (q, BOLD_TAG, INT_DENOTATION, STOP)
			   || a68_whether (q, OPERATOR, INT_DENOTATION, STOP)
			   || a68_whether (q, EQUALS_SYMBOL, INT_DENOTATION, STOP))
		{
		  /* The scanner cannot separate operator and "=" sign so we do this here.  */
		  int len = (int) strlen (NSYMBOL (q));
		  if (len > 1 && NSYMBOL (q)[len - 1] == '=')
		    {
		      NODE_T *y = q;
		      char *sym = (char *) xmalloc ((size_t) (len + 1));
		      a68_bufcpy (sym, NSYMBOL (q), len + 1);
		      sym[len - 1] = '\0';
		      NSYMBOL (q) = TEXT (a68_add_token (&A68 (top_token), sym));
		      free (sym);
		      if (len > 2 && NSYMBOL (q)[len - 2] == ':' && NSYMBOL (q)[len - 3] != '=')
			a68_error (q, "probably a missing symbol near invalid operator S");
		      ATTRIBUTE (q) = DEFINING_OPERATOR;
		      PUBLICIZED (q) = is_public;
		      insert_alt_equals (q);
		      q = NEXT_NEXT (q);
		      int k;
		      GET_PRIORITY (q, k);
		      ATTRIBUTE (q) = PRIORITY;
		      if (a68_add_tag (TABLE (p), PRIO_SYMBOL, y, NO_MOID, k) == NO_TAG)
			gcc_unreachable ();
		      FORWARD (q);
		    }
		  else
		    siga = false;
		}
	      else
		siga = false;
	    }
	  while (siga && q != NO_NODE && IS (q, COMMA_SYMBOL));
	}
      else
	FORWARD (q);
    }
}

/* Search OP [( .. ) ..] X = .., Y = .. and store operators.  */

void
a68_extract_operators (NODE_T *p)
{
  NODE_T *q = p;

  while (q != NO_NODE)
    {
      if (IS (q, OP_SYMBOL) || a68_whether (q, PUBLIC_SYMBOL, OP_SYMBOL, STOP))
	{
	  bool siga = true;
	  bool in_proc = true;
	  bool is_public = false;

	  if (IS (q, PUBLIC_SYMBOL))
	    {
	      is_public = true;
	      FORWARD (q);
	    }

	  /* Skip operator plan.  */
	  if (NEXT (q) != NO_NODE && IS (NEXT (q), OPEN_SYMBOL))
	    {
	      q = skip_pack_declarer (NEXT (q));
	      in_proc = false;
	    }
	  /* Sample operators.  */
	  if (q != NO_NODE)
	    {
	      do
		{
		  FORWARD (q);
		  detect_redefined_keyword (q, OPERATOR_DECLARATION);
		  /* Unacceptable operator tags like ++ or && could give
		     strange errors.  */
		  if (a68_whether (q, OPERATOR, OPERATOR, STOP))
		    {
		      a68_error (q, "invalid operator tag");
		      ATTRIBUTE (q) = DEFINING_OPERATOR;
		      PUBLICIZED (q) = is_public;
		      TAG_T *t = a68_add_tag (TABLE (p), OP_SYMBOL, q, NO_MOID, STOP);
		      if (t == NO_TAG)
			gcc_unreachable ();
		      IN_PROC (t) = in_proc;
		      /* Remove one superfluous operator, and hope it was only one.  */
		      NEXT (q) = NEXT_NEXT (q);
		      PREVIOUS (NEXT (q)) = q;
		      FORWARD (q);
		      ATTRIBUTE (q) = ALT_EQUALS_SYMBOL;
		      q = skip_unit (q);
		    }
		  else if (a68_whether (q, OPERATOR, EQUALS_SYMBOL, STOP)
			   || a68_whether (q, EQUALS_SYMBOL, EQUALS_SYMBOL, STOP))
		    {
		      ATTRIBUTE (q) = DEFINING_OPERATOR;
		      PUBLICIZED (q) = is_public;
		      TAG_T *t = a68_add_tag (TABLE (p), OP_SYMBOL, q, NO_MOID, STOP);
		      if (t == NO_TAG)
			gcc_unreachable ();
		      IN_PROC (t) = in_proc;
		      FORWARD (q);
		      ATTRIBUTE (q) = ALT_EQUALS_SYMBOL;
		      q = skip_unit (q);
		    }
		  else if (a68_whether (q, BOLD_TAG, IDENTIFIER, STOP))
		    {
		      siga = false;
		    }
		  else if (a68_whether (q, BOLD_TAG, EQUALS_SYMBOL, STOP))
		    {
		      ATTRIBUTE (q) = DEFINING_OPERATOR;
		      PUBLICIZED (q) = is_public;
		      TAG_T *t = a68_add_tag (TABLE (p), OP_SYMBOL, q, NO_MOID, STOP);
		      if (t == NO_TAG)
			gcc_unreachable ();
		      IN_PROC (t) = in_proc;
		      FORWARD (q);
		      ATTRIBUTE (q) = ALT_EQUALS_SYMBOL;
		      q = skip_unit (q);
		    }
		  else if (q != NO_NODE && (a68_is_one_of (q, OPERATOR, BOLD_TAG, EQUALS_SYMBOL, STOP)))
		    {
		      /* The scanner cannot separate operator and "=" sign so
			 we do this here.  */
		      int len = (int) strlen (NSYMBOL (q));
		      if (len > 1 && NSYMBOL (q)[len - 1] == '=')
			{
			  char *sym = (char *) xmalloc ((size_t) (len + 1));
			  a68_bufcpy (sym, NSYMBOL (q), len + 1);
			  sym[len - 1] = '\0';
			  NSYMBOL (q) = TEXT (a68_add_token (&A68 (top_token), sym));
			  if (len > 2 && NSYMBOL (q)[len - 2] == ':' && NSYMBOL (q)[len - 3] != '=')
			    a68_error (q, "probably a missing symbol near invalid operator S");
			  ATTRIBUTE (q) = DEFINING_OPERATOR;
			  PUBLICIZED (q) = is_public;
			  insert_alt_equals (q);
			  TAG_T *t = a68_add_tag (TABLE (p), OP_SYMBOL, q, NO_MOID, STOP);
			  if (t == NO_TAG)
			    gcc_unreachable ();
			  IN_PROC (t) = in_proc;
			  FORWARD (q);
			  q = skip_unit (q);
			}
		      else
			siga = false;
		    }
		  else
		    siga = false;
		}
	      while (siga && q != NO_NODE && IS (q, COMMA_SYMBOL));
	    }
	}
      else
	FORWARD (q);
    }
}

/* Search and store labels.  */

void
a68_extract_labels (NODE_T *p, int expect)
{
  /* Only handle candidate phrases as not to search indexers!.  */
  if (expect == SERIAL_CLAUSE || expect == ENQUIRY_CLAUSE || expect == SOME_CLAUSE)
    {
      for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
	{
	  if (a68_whether (q, IDENTIFIER, COLON_SYMBOL, STOP))
	    {
	      TAG_T *z = a68_add_tag (TABLE (p), LABEL, q, NO_MOID, LOCAL_LABEL);
	      ATTRIBUTE (q) = DEFINING_IDENTIFIER;
	      UNIT (z) = NO_NODE;
	    }
	}
    }
}

/* Search MOID x = .., y = .. and store identifiers.  */

static void
extract_identities (NODE_T *p)
{
  NODE_T *q = p;

  while (q != NO_NODE)
    {
      if (a68_whether (q, DECLARER, IDENTIFIER, EQUALS_SYMBOL, STOP)
	  || a68_whether (q, PUBLIC_SYMBOL, DECLARER, IDENTIFIER, EQUALS_SYMBOL, STOP))
	{
	  bool siga = true;
	  bool is_public = false;

	  if (IS (q, PUBLIC_SYMBOL))
	    {
	      is_public = true;
	      FORWARD (q);
	    }

	  do
	    {
	      if (a68_whether ((FORWARD (q)), IDENTIFIER, EQUALS_SYMBOL, STOP))
		{
		  TAG_T *tag = a68_add_tag (TABLE (p), IDENTIFIER, q, NO_MOID, NORMAL_IDENTIFIER);
		  if (tag == NO_TAG)
		    gcc_unreachable ();
		  ATTRIBUTE (q) = DEFINING_IDENTIFIER;
		  PUBLICIZED (q) = is_public;
		  FORWARD (q);
		  ATTRIBUTE (q) = ALT_EQUALS_SYMBOL;
		  q = skip_unit (q);
		}
	      else if (a68_whether (q, IDENTIFIER, ASSIGN_SYMBOL, STOP))
		{
		  /* Handle common error in ALGOL 68 programs.  */
		  a68_error (q, "mixed identity-declaration and variable-declaration");
		  if (a68_add_tag (TABLE (p), IDENTIFIER, q, NO_MOID, NORMAL_IDENTIFIER) == NO_TAG)
		    gcc_unreachable ();
		  ATTRIBUTE (q) = DEFINING_IDENTIFIER;
		  PUBLICIZED (q) = is_public;
		  ATTRIBUTE (FORWARD (q)) = ALT_EQUALS_SYMBOL;
		  q = skip_unit (q);
		}
	      else
		siga = false;
	    }
	  while (siga && q != NO_NODE && IS (q, COMMA_SYMBOL));
	}
      else
	FORWARD (q);
    }
}

/* Search MOID x [:= ..], y [:= ..] and store identifiers.  */

static void
extract_variables (NODE_T *p)
{
  NODE_T *q = p;

  while (q != NO_NODE)
    {
      if (a68_whether (q, HEAP_SYMBOL, DECLARER, IDENTIFIER, STOP)
	  || a68_whether (q, LOC_SYMBOL, DECLARER, IDENTIFIER, STOP)
	  || a68_whether (q, DECLARER, IDENTIFIER, STOP)
	  || a68_whether (q, PUBLIC_SYMBOL, HEAP_SYMBOL, DECLARER, IDENTIFIER, STOP)
	  || a68_whether (q, PUBLIC_SYMBOL, LOC_SYMBOL, DECLARER, IDENTIFIER, STOP)
	  || a68_whether (q, PUBLIC_SYMBOL, DECLARER, IDENTIFIER, STOP))
	{
	  bool is_public = IS (q, PUBLIC_SYMBOL);
	  while (!IS (q, DECLARER))
	    FORWARD (q);

	  bool siga = true;
	  do
	    {
	      FORWARD (q);
	      if (a68_whether (q, IDENTIFIER, STOP))
		{
		  if (a68_whether (q, IDENTIFIER, EQUALS_SYMBOL, STOP))
		    {
		      /* Handle common error in ALGOL 68 programs.  */
		      a68_error (q, "mixed identity-declaration and variable-declaration");
		      ATTRIBUTE (NEXT (q)) = ASSIGN_SYMBOL;
		    }
		  TAG_T *tag = a68_add_tag (TABLE (p), IDENTIFIER, q, NO_MOID, NORMAL_IDENTIFIER);
		  if (tag == NO_TAG)
		    gcc_unreachable ();
		  VARIABLE (tag) = true;
		  ATTRIBUTE (q) = DEFINING_IDENTIFIER;
		  PUBLICIZED (q) = is_public;
		  q = skip_unit (q);
		}
	      else
		siga = false;
	    }
	  while (siga && q != NO_NODE && IS (q, COMMA_SYMBOL));
	}
      else
	FORWARD (q);
    }
}

/* Search PROC x = .., y = .. and stores identifiers.  */

static void
extract_proc_identities (NODE_T *p)
{
  NODE_T *q = p;

  while (q != NO_NODE)
    {
      if (a68_whether (q, PROC_SYMBOL, IDENTIFIER, EQUALS_SYMBOL, STOP)
	  || a68_whether (q, PUBLIC_SYMBOL, PROC_SYMBOL, IDENTIFIER, EQUALS_SYMBOL, STOP))
	{
	  bool siga = true;
	  bool is_public = false;

	  if (IS (q, PUBLIC_SYMBOL))
	    {
	      is_public = true;
	      FORWARD (q);
	    }

	  do
	    {
	      FORWARD (q);
	      if (a68_whether (q, IDENTIFIER, EQUALS_SYMBOL, STOP))
		{
		  TAG_T *t = a68_add_tag (TABLE (p), IDENTIFIER, q, NO_MOID, NORMAL_IDENTIFIER);
		  IN_PROC (t) = true;
		  ATTRIBUTE (q) = DEFINING_IDENTIFIER;
		  PUBLICIZED (q) = is_public;
		  ATTRIBUTE (FORWARD (q)) = ALT_EQUALS_SYMBOL;
		  q = skip_unit (q);
		}
	      else if (a68_whether (q, IDENTIFIER, ASSIGN_SYMBOL, STOP))
		{
		  /* Handle common error in ALGOL 68 programs. */
		  a68_error (q, "mixed identity-declaration and variable-declaration");
		  if (a68_add_tag (TABLE (p), IDENTIFIER, q, NO_MOID, NORMAL_IDENTIFIER) == NO_TAG)
		    gcc_unreachable ();
		  ATTRIBUTE (q) = DEFINING_IDENTIFIER;
		  PUBLICIZED (q) = is_public;
		  ATTRIBUTE (FORWARD (q)) = ALT_EQUALS_SYMBOL;
		  q = skip_unit (q);
		}
	      else
		siga = false;
	    }
	  while (siga && q != NO_NODE && IS (q, COMMA_SYMBOL));
	}
      else
	FORWARD (q);
    }
}

/* Search PROC x [:= ..], y [:= ..]; store identifiers.  */

static void
extract_proc_variables (NODE_T *p)
{
  NODE_T *q = p;

  while (q != NO_NODE)
    {
      if (a68_whether (q, PROC_SYMBOL, IDENTIFIER, STOP)
	  || a68_whether (q, PUBLIC_SYMBOL, PROC_SYMBOL, IDENTIFIER, STOP))
	{
	  bool siga = true;
	  bool is_public = false;

	  if (IS (q, PUBLIC_SYMBOL))
	    {
	      is_public = true;
	      FORWARD (q);
	    }

	  do
	    {
	      FORWARD (q);
	      if (a68_whether (q, IDENTIFIER, ASSIGN_SYMBOL, STOP))
		{
		  TAG_T *tag = a68_add_tag (TABLE (p), IDENTIFIER, q, NO_MOID, NORMAL_IDENTIFIER);
		  gcc_assert (tag != NO_TAG);
		  VARIABLE (tag) = true;
		  ATTRIBUTE (q) = DEFINING_IDENTIFIER;
		  PUBLICIZED (q) = is_public;
		  q = skip_unit (FORWARD (q));
		}
	      else if (a68_whether (q, IDENTIFIER, EQUALS_SYMBOL, STOP))
		{
		  /* Handle common error in ALGOL 68 programs.  */
		  a68_error (q, "mixed identity-declaration and variable-declaration");
		  TAG_T *tag = a68_add_tag (TABLE (p), IDENTIFIER, q, NO_MOID, NORMAL_IDENTIFIER);
		  gcc_assert (tag != NO_TAG);
		  ATTRIBUTE (q) = DEFINING_IDENTIFIER;
		  PUBLICIZED (q) = is_public;
		  ATTRIBUTE (FORWARD (q)) = ASSIGN_SYMBOL;
		  q = skip_unit (q);
		} else
		siga = false;
	    }
	  while (siga && q != NO_NODE && IS (q, COMMA_SYMBOL));
	}
      else
	FORWARD (q);
    }
}

/* Schedule gathering of definitions in a phrase.  */

void
a68_extract_declarations (NODE_T *p)
{
  /* Get definitions so we know what is defined in this range.  */
  extract_identities (p);
  extract_variables (p);
  extract_proc_identities (p);
  extract_proc_variables (p);
  /* By now we know whether "=" is an operator or not.  */
  for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
    {
      if (IS (q, EQUALS_SYMBOL))
	ATTRIBUTE (q) = OPERATOR;
      else if (IS (q, ALT_EQUALS_SYMBOL))
	ATTRIBUTE (q) = EQUALS_SYMBOL;
    }

  /* Get qualifiers.  */
  for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
    {
      if (a68_whether (q, LOC_SYMBOL, DECLARER, DEFINING_IDENTIFIER, STOP))
	a68_make_sub (q, q, QUALIFIER);
      if (a68_whether (q, HEAP_SYMBOL, DECLARER, DEFINING_IDENTIFIER, STOP))
	a68_make_sub (q, q, QUALIFIER);
      if (a68_whether (q, LOC_SYMBOL, PROC_SYMBOL, DEFINING_IDENTIFIER, STOP))
	a68_make_sub (q, q, QUALIFIER);
      if (a68_whether (q, HEAP_SYMBOL, PROC_SYMBOL, DEFINING_IDENTIFIER, STOP))
	a68_make_sub (q, q, QUALIFIER);
    }

  /* Give priorities to operators.  */
  for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
    {
      if (IS (q, OPERATOR))
	{
	  if (a68_find_tag_global (TABLE (q), OP_SYMBOL, NSYMBOL (q)))
	    {
	      TAG_T *s = a68_find_tag_global (TABLE (q), PRIO_SYMBOL, NSYMBOL (q));

	      if (s != NO_TAG)
		PRIO (INFO (q)) = PRIO (s);
	      else
		PRIO (INFO (q)) = 0;
	    }
	  else
	    {
	      a68_error (q, "tag S has not been declared properly");
	      PRIO (INFO (q)) = 1;
	    }
	}
    }
}
