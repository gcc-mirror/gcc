/* Implementation of -Wmisleading-indentation
   Copyright (C) 2015-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "c-common.h"
#include "c-indentation.h"

extern cpp_options *cpp_opts;

/* Round up VIS_COLUMN to nearest tab stop. */

static unsigned int
next_tab_stop (unsigned int vis_column)
{
  const unsigned int tab_width = cpp_opts->tabstop;
  vis_column = ((vis_column + tab_width) / tab_width) * tab_width;
  return vis_column;
}

/* Convert libcpp's notion of a column (a 1-based char count) to
   the "visual column" (0-based column, respecting tabs), by reading the
   relevant line.

   Returns true if a conversion was possible, writing the result to OUT,
   otherwise returns false.  If FIRST_NWS is not NULL, then write to it
   the visual column corresponding to the first non-whitespace character
   on the line.  */

static bool
get_visual_column (expanded_location exploc, location_t loc,
		   unsigned int *out,
		   unsigned int *first_nws)
{
  /* PR c++/68819: if the column number is zero, we presumably
     had a location_t > LINE_MAP_MAX_LOCATION_WITH_COLS, and so
     we have no column information.
     Act as if no conversion was possible, triggering the
     error-handling path in the caller.  */
  if (!exploc.column)
    {
      static bool issued_note = false;
      if (!issued_note)
	{
	  /* Notify the user the first time this happens.  */
	  issued_note = true;
	  inform (loc,
		  "-Wmisleading-indentation is disabled from this point"
		  " onwards, since column-tracking was disabled due to"
		  " the size of the code/headers");
	}
      return false;
    }

  int line_len;
  const char *line = location_get_source_line (exploc.file, exploc.line,
					       &line_len);
  if (!line)
    return false;
  unsigned int vis_column = 0;
  for (int i = 1; i < exploc.column; i++)
    {
      unsigned char ch = line[i - 1];

      if (first_nws != NULL && !ISSPACE (ch))
	{
	  *first_nws = vis_column;
	  first_nws = NULL;
	}

      if (ch == '\t')
	vis_column = next_tab_stop (vis_column);
      else
       vis_column++;
    }

  if (first_nws != NULL)
    *first_nws = vis_column;

  *out = vis_column;
  return true;
}

/* Attempt to determine the first non-whitespace character in line LINE_NUM
   of source line FILE.

   If this is possible, return true and write its "visual column" to
   *FIRST_NWS.
   Otherwise, return false, leaving *FIRST_NWS untouched.  */

static bool
get_first_nws_vis_column (const char *file, int line_num,
			  unsigned int *first_nws)
{
  gcc_assert (first_nws);

  int line_len;
  const char *line = location_get_source_line (file, line_num, &line_len);
  if (!line)
    return false;
  unsigned int vis_column = 0;
  for (int i = 1; i < line_len; i++)
    {
      unsigned char ch = line[i - 1];

      if (!ISSPACE (ch))
	{
	  *first_nws = vis_column;
	  return true;
	}

      if (ch == '\t')
	vis_column = next_tab_stop (vis_column);
      else
	vis_column++;
    }

  /* No non-whitespace characters found.  */
  return false;
}

/* Determine if there is an unindent/outdent between
   BODY_EXPLOC and NEXT_STMT_EXPLOC, to ensure that we don't
   issue a warning for cases like the following:

   (1) Preprocessor logic

	if (flagA)
	  foo ();
	  ^ BODY_EXPLOC
      #if SOME_CONDITION_THAT_DOES_NOT_HOLD
	if (flagB)
      #endif
	  bar ();
	  ^ NEXT_STMT_EXPLOC

   "bar ();" is visually aligned below "foo ();" and
   is (as far as the parser sees) the next token, but
   this isn't misleading to a human reader.

   (2) Empty macro with bad indentation

   In the following, the
     "if (i > 0)"
   is poorly indented, and ought to be on the same column as
      "engine_ref_debug(e, 0, -1)"
   However, it is not misleadingly indented, due to the presence
   of that macro.

      #define engine_ref_debug(X, Y, Z)

      if (locked)
        i = foo (0);
      else
        i = foo (1);
      engine_ref_debug(e, 0, -1)
        if (i > 0)
        return 1;

   Return true if such an unindent/outdent is detected.  */

static bool
detect_intervening_unindent (const char *file,
			     int body_line,
			     int next_stmt_line,
			     unsigned int vis_column)
{
  gcc_assert (file);
  gcc_assert (next_stmt_line > body_line);

  for (int line = body_line + 1; line < next_stmt_line; line++)
    {
      unsigned int line_vis_column;
      if (get_first_nws_vis_column (file, line, &line_vis_column))
	if (line_vis_column < vis_column)
	  return true;
    }

  /* Not found.  */
  return false;
}


/* Helper function for warn_for_misleading_indentation; see
   description of that function below.  */

static bool
should_warn_for_misleading_indentation (const token_indent_info &guard_tinfo,
					const token_indent_info &body_tinfo,
					const token_indent_info &next_tinfo)
{
  location_t guard_loc = guard_tinfo.location;
  location_t body_loc = body_tinfo.location;
  location_t next_stmt_loc = next_tinfo.location;

  enum cpp_ttype body_type = body_tinfo.type;
  enum cpp_ttype next_tok_type = next_tinfo.type;

  /* Don't attempt to compare the indentation of BODY_LOC and NEXT_STMT_LOC
     if either are within macros.  */
  if (linemap_location_from_macro_expansion_p (line_table, body_loc)
      || linemap_location_from_macro_expansion_p (line_table, next_stmt_loc))
    return false;

  /* Don't attempt to compare indentation if #line or # 44 "file"-style
     directives are present, suggesting generated code.

     All bets are off if these are present: the file that the #line
     directive could have an entirely different coding layout to C/C++
     (e.g. .md files).

     To determine if a #line is present, in theory we could look for a
     map with reason == LC_RENAME_VERBATIM.  However, if there has
     subsequently been a long line requiring a column number larger than
     that representable by the original LC_RENAME_VERBATIM map, then
     we'll have a map with reason LC_RENAME.
     Rather than attempting to search all of the maps for a
     LC_RENAME_VERBATIM, instead we have libcpp set a flag whenever one
     is seen, and we check for the flag here.
  */
  if (line_table->seen_line_directive)
    return false;

  /* We can't usefully warn about do-while and switch statements since the
     bodies of these statements are always explicitly delimited at both ends,
     so control flow is quite obvious.  */
  if (guard_tinfo.keyword == RID_DO
      || guard_tinfo.keyword == RID_SWITCH)
    return false;

  /* If the token following the body is a close brace or an "else"
     then while indentation may be sloppy, there is not much ambiguity
     about control flow, e.g.

     if (foo)       <- GUARD
       bar ();      <- BODY
       else baz (); <- NEXT

     {
     while (foo)  <- GUARD
     bar ();      <- BODY
     }            <- NEXT
     baz ();
  */
  if (next_tok_type == CPP_CLOSE_BRACE
      || next_tinfo.keyword == RID_ELSE)
    return false;

  /* Likewise, if the body of the guard is a compound statement then control
     flow is quite visually explicit regardless of the code's possibly poor
     indentation, e.g.

     while (foo)  <- GUARD
       {          <- BODY
       bar ();
       }
       baz ();    <- NEXT

    Things only get muddy when the body of the guard does not have
    braces, e.g.

    if (foo)  <- GUARD
      bar (); <- BODY
      baz (); <- NEXT
  */
  if (body_type == CPP_OPEN_BRACE)
    return false;

  /* Don't warn here about spurious semicolons.  */
  if (next_tok_type == CPP_SEMICOLON)
    return false;

  expanded_location body_exploc = expand_location (body_loc);
  expanded_location next_stmt_exploc = expand_location (next_stmt_loc);
  expanded_location guard_exploc = expand_location (guard_loc);

  /* They must be in the same file.  */
  if (next_stmt_exploc.file != body_exploc.file)
    return false;

  /* If NEXT_STMT_LOC and BODY_LOC are on the same line, consider
     the location of the guard.

     Cases where we want to issue a warning:

       if (flag)
         foo ();  bar ();
                  ^ WARN HERE

       if (flag) foo (); bar ();
                         ^ WARN HERE


       if (flag) ; {
                   ^ WARN HERE

       if (flag)
        ; {
          ^ WARN HERE

     Cases where we don't want to issue a warning:

       various_code (); if (flag) foo (); bar (); more_code ();
                                          ^ DON'T WARN HERE.  */
  if (next_stmt_exploc.line == body_exploc.line)
    {
      if (guard_exploc.file != body_exploc.file)
	return true;
      if (guard_exploc.line < body_exploc.line)
	/* The guard is on a line before a line that contains both
	   the body and the next stmt.  */
	return true;
      else if (guard_exploc.line == body_exploc.line)
	{
	  /* They're all on the same line.  */
	  gcc_assert (guard_exploc.file == next_stmt_exploc.file);
	  gcc_assert (guard_exploc.line == next_stmt_exploc.line);
	  unsigned int guard_vis_column;
	  unsigned int guard_line_first_nws;
	  if (!get_visual_column (guard_exploc, guard_loc,
				  &guard_vis_column,
				  &guard_line_first_nws))
	    return false;
	  /* Heuristic: only warn if the guard is the first thing
	     on its line.  */
	  if (guard_vis_column == guard_line_first_nws)
	    return true;
	}
    }

  /* If NEXT_STMT_LOC is on a line after BODY_LOC, consider
     their relative locations, and of the guard.

     Cases where we want to issue a warning:
        if (flag)
          foo ();
          bar ();
          ^ WARN HERE

     Cases where we don't want to issue a warning:
        if (flag)
        foo ();
        bar ();
        ^ DON'T WARN HERE (autogenerated code?)

	if (flagA)
	  foo ();
      #if SOME_CONDITION_THAT_DOES_NOT_HOLD
	if (flagB)
      #endif
	  bar ();
	  ^ DON'T WARN HERE

	if (flag)
	  ;
	  foo ();
	  ^ DON'T WARN HERE

	#define emit
	if (flag)
	     foo ();
	emit bar ();
	     ^ DON'T WARN HERE

  */
  if (next_stmt_exploc.line > body_exploc.line)
    {
      /* Determine if GUARD_LOC and NEXT_STMT_LOC are aligned on the same
	 "visual column"...  */
      unsigned int next_stmt_vis_column;
      unsigned int next_stmt_line_first_nws;
      unsigned int body_vis_column;
      unsigned int body_line_first_nws;
      unsigned int guard_vis_column;
      unsigned int guard_line_first_nws;
      /* If we can't determine it, don't issue a warning.  This is sometimes
	 the case for input files containing #line directives, and these
	 are often for autogenerated sources (e.g. from .md files), where
	 it's not clear that it's meaningful to look at indentation.  */
      if (!get_visual_column (next_stmt_exploc, next_stmt_loc,
			      &next_stmt_vis_column,
			      &next_stmt_line_first_nws))
	return false;
      if (!get_visual_column (body_exploc, body_loc,
			      &body_vis_column,
			      &body_line_first_nws))
	return false;
      if (!get_visual_column (guard_exploc, guard_loc,
			      &guard_vis_column,
			      &guard_line_first_nws))
	return false;

      /* If the line where the next stmt starts has non-whitespace
	 on it before the stmt, then don't warn:
	  #define emit
	  if (flag)
	       foo ();
	  emit bar ();
	       ^ DON'T WARN HERE
	 (PR c/69122).  */
      if (next_stmt_line_first_nws < next_stmt_vis_column)
	return false;

      if ((body_type != CPP_SEMICOLON
	   && next_stmt_vis_column == body_vis_column)
	  /* As a special case handle the case where the body is a semicolon
	     that may be hidden by a preceding comment, e.g.  */

	  // if (p)
	  //   /* blah */;
	  //   foo (1);

	  /*  by looking instead at the column of the first non-whitespace
	      character on the body line.  */
	  || (body_type == CPP_SEMICOLON
	      && body_exploc.line > guard_exploc.line
	      && body_line_first_nws != body_vis_column
	      && next_stmt_vis_column > guard_line_first_nws))
	{
          /* Don't warn if they are aligned on the same column
	     as the guard itself (suggesting autogenerated code that doesn't
	     bother indenting at all).
	     For "else" clauses, we consider the column of the first
	     non-whitespace character on the guard line instead of the column
	     of the actual guard token itself because it is more sensible.
	     Consider:

	     if (p) {
	     foo (1);
	     } else     // GUARD
	     foo (2);   // BODY
	     foo (3);   // NEXT

	     and:

	     if (p)
	       foo (1);
	     } else       // GUARD
	       foo (2);   // BODY
	       foo (3);   // NEXT

	     If we just used the column of the "else" token, we would warn on
	     the first example and not warn on the second.  But we want the
	     exact opposite to happen: to not warn on the first example (which
	     is probably autogenerated) and to warn on the second (whose
	     indentation is misleading).  Using the column of the first
	     non-whitespace character on the guard line makes that
	     happen.  */
	  unsigned int guard_column = (guard_tinfo.keyword == RID_ELSE
				       ? guard_line_first_nws
				       : guard_vis_column);
	  if (guard_column == body_vis_column)
	    return false;

	  /* We may have something like:

	     if (p)
	       {
	       foo (1);
	       } else  // GUARD
	     foo (2);  // BODY
	     foo (3);  // NEXT

	     in which case the columns are not aligned but the code is not
	     misleadingly indented.  If the column of the body isn't indented
	     more than the guard line then don't warn.  */
	  if (body_vis_column <= guard_line_first_nws)
	    return false;

	  /* Don't warn if there is an unindent between the two statements. */
	  int vis_column = MIN (next_stmt_vis_column, body_vis_column);
	  if (detect_intervening_unindent (body_exploc.file, body_exploc.line,
					   next_stmt_exploc.line,
					   vis_column))
	    return false;

	  /* Otherwise, they are visually aligned: issue a warning.  */
	  return true;
	}

	/* Also issue a warning for code having the form:

	   if (flag);
	     foo ();

	   while (flag);
	   {
	     ...
	   }

	   for (...);
	     {
	       ...
	     }

	   if (flag)
	     ;
	   else if (flag);
	     foo ();

	   where the semicolon at the end of each guard is most likely spurious.

	   But do not warn on:

	   for (..);
	   foo ();

	   where the next statement is aligned with the guard.
	*/
	if (body_type == CPP_SEMICOLON)
	  {
	    if (body_exploc.line == guard_exploc.line)
	      {
		if (next_stmt_vis_column > guard_line_first_nws
		    || (next_tok_type == CPP_OPEN_BRACE
			&& next_stmt_vis_column == guard_line_first_nws))
		  return true;
	      }
	  }
    }

  return false;
}

/* Return the string identifier corresponding to the given guard token.  */

static const char *
guard_tinfo_to_string (const token_indent_info &guard_tinfo)
{
  switch (guard_tinfo.keyword)
    {
    case RID_FOR:
      return "for";
    case RID_ELSE:
      return "else";
    case RID_IF:
      return "if";
    case RID_WHILE:
      return "while";
    case RID_DO:
      return "do";
    default:
      gcc_unreachable ();
    }
}

/* Called by the C/C++ frontends when we have a guarding statement at
   GUARD_LOC containing a statement at BODY_LOC, where the block wasn't
   written using braces, like this:

     if (flag)
       foo ();

   along with the location of the next token, at NEXT_STMT_LOC,
   so that we can detect followup statements that are within
   the same "visual block" as the guarded statement, but which
   aren't logically grouped within the guarding statement, such
   as:

     GUARD_LOC
     |
     V
     if (flag)
       foo (); <- BODY_LOC
       bar (); <- NEXT_STMT_LOC

   In the above, "bar ();" isn't guarded by the "if", but
   is indented to misleadingly suggest that it is in the same
   block as "foo ();".

   GUARD_KIND identifies the kind of clause e.g. "if", "else" etc.  */

void
warn_for_misleading_indentation (const token_indent_info &guard_tinfo,
				 const token_indent_info &body_tinfo,
				 const token_indent_info &next_tinfo)
{
  /* Early reject for the case where -Wmisleading-indentation is disabled,
     to avoid doing work only to have the warning suppressed inside the
     diagnostic machinery.  */
  if (!warn_misleading_indentation)
    return;

  if (should_warn_for_misleading_indentation (guard_tinfo,
					      body_tinfo,
					      next_tinfo))
    {
      if (warning_at (guard_tinfo.location, OPT_Wmisleading_indentation,
		      "this %qs clause does not guard...",
		      guard_tinfo_to_string (guard_tinfo)))
	inform (next_tinfo.location,
		"...this statement, but the latter is misleadingly indented"
		" as if it were guarded by the %qs",
		guard_tinfo_to_string (guard_tinfo));
    }
}
