
/*

   Test to see if a particular fix should be applied to a header file.

   Copyright (C) 1997-1999, 2000 Free Software Foundation, Inc.

= = = = = = = = = = = = = = = = = = = = = = = = =

NOTE TO DEVELOPERS

The routines you write here must work closely with both the fixincl.c
and the test_need.c program.

Here are the rules:

1.  Every test procedure name must be suffixed with "_fix".
    These routines will be referenced from inclhack.def, sans the suffix.

2.  Use the "FIX_PROC_HEAD()" macro _with_ the "_fix" suffix
    (I cannot use the ## magic from ANSI C) for defining your entry point.

3.  Put your test name into the FIXUP_TABLE

4.  Do not read anything from stdin.  It is closed.

5.  Write to stderr only in the event of a reportable error
    In such an event, call "exit(1)".

6.  If "MAIN" is _not_ defined, then you have access to the fixDescList
    entry for the fix in question.  This may be useful, for example,
    if there are pre-compiled selection expressions stored there.

    For example, you may do this if you know that the first 
    test contains a useful regex.  This is okay because, remember,
    this code perforce works closely with the inclhack.def fixes!!


    tFixDesc*  pMyDesc = fixDescList + MY_FIX_NAME_FIXIDX;
    tTestDesc* pTestList = pMyDesc->p_test_desc;

    regexec (pTestList->p_test_regex, ...)


    If MAIN _is_ defined, then you will have to compile it on
    your own.

= = = = = = = = = = = = = = = = = = = = = = = = =

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "fixlib.h"

typedef struct {
    const char*  fix_name;
    void (*fix_proc)();
} fix_entry_t;

#define FIXUP_TABLE \
  _FT_( "no_double_slash",  double_slash_fix ) \
  _FT_( "else_endif_label", else_endif_label_fix ) \
  _FT_( "IO_use",	    IO_use_fix ) \
  _FT_( "CTRL_use",	    CTRL_use_fix) \
  _FT_( "IO_defn",	    IO_defn_fix ) \
  _FT_( "CTRL_defn",	    CTRL_defn_fix ) \
  _FT_( "machine_name",	    machine_name_fix )


#define FIX_PROC_HEAD( fix ) \
static void fix ( filname, text ) \
    const char* filname; \
    char* text;


/*
 *  Skip over a quoted string.  Single quote strings may
 *  contain multiple characters if the first character is
 *  a backslash.  Especially a backslash followed by octal digits.
 *  We are not doing a correctness syntax check here.
 */
static char*
print_quote( q, text )
  char  q;
  char* text;
{
  fputc( q, stdout );

  for (;;)
    {
      char ch = *(text++);
      fputc( ch, stdout );

      switch (ch)
        {
        case '\\':
          if (*text == NUL)
            goto quote_done;

          fputc( *(text++), stdout );
          break;

        case '"':
        case '\'':
          if (ch != q)
            break;
          /*FALLTHROUGH*/

        case '\n':
        case NUL:
          goto quote_done;
        }
    } quote_done:;

  return text;
}


FIX_PROC_HEAD( double_slash_fix )
{
  /*  Now look for the comment markers in the text */
  for (;;)
    {
      char ch = *(text++);
      switch (ch)
        {
        case '/':
          switch (*text) /* do not advance `text' here */
            {
            case '/':
              /*
                We found a "//" pair in open text.
                Delete text to New-Line
              */
              while ((*text != '\n') && (*text != '\0'))  text++;
              break;

            case '*':
              {
                /* We found a C-style comment.  Skip forward to the end */
                char* pz = strstr( (--text)+2, "*/" );
                if (pz == (char*)NULL)
                  {
                    fputs( text, stdout );
                    goto fix_done;
                  }
                pz += 2;
                fwrite (text, (pz - text), 1, stdout );
                text = pz;
              }
              break;

            default:
              fputc (ch, stdout );
            }
          break;

        case NUL:
          goto fix_done;

        case '"':
        case '\'':
          text = print_quote (ch, text );
          break;

        default:
          fputc (ch, stdout );
        }

    } fix_done:;

  fclose (stdout);;
}


FIX_PROC_HEAD( else_endif_label_fix )
{
  static const char label_pat[] = "^[ \t]*#[ \t]*(else|endif)";
  static regex_t label_re;

  char ch;
  char* pz_next = (char*)NULL;
  regmatch_t match[2];

  compile_re (label_pat, &label_re, 1,
	      "label pattern", "else_endif_label_fix");

  for (;;) /* entire file */
    {
      /*
        See if we need to advance to the next candidate directive
        If the scanning pointer passes over the end of the directive,
        then the directive is inside a comment */
      if (pz_next < text)
        {
          if (regexec (&label_re, text, 2, match, 0) != 0)
            {
              fputs( text, stdout );
              break;
            }

          pz_next = text + match[0].rm_eo;
        }

      /*
        IF the scan pointer has not reached the directive end, ... */
      if (pz_next > text)
        {
          /*
            Advance the scanning pointer.  If we are at the start
            of a quoted string or a comment, then skip the entire unit */
          ch = *text;

          switch (ch)
            {
            case '/':
              /*
                Skip comments */
              if (text[1] == '*')
                {
                  char* pz = strstr( text+2, "*/" );
                  if (pz == (char*)NULL)
                    {
                      fputs( text, stdout );
                      return;
                    }
                  pz += 2;
                  fwrite( text, 1, (pz - text), stdout );
                  text = pz;
                  continue;
                }
              putc( ch, stdout );
              text++;
              break;

            case '"':
            case '\'':
              text = print_quote( ch, text+1 );
              break;

            default:
              putc( ch, stdout );
              text++;
            } /* switch (ch) */
          continue;
        } /* if (still shy of directive end) */

      /*
         The scanning pointer (text) has reached the end of the current
         directive under test.  Check for bogons here.  */
      for (;;) /* bogon check */
        {
          char ch = *(text++);
          if (isspace (ch))
            {
              putc( ch, stdout );
              if (ch == '\n')
                {
                  /*
                    It is clean.  No bogons on this directive */
                  pz_next = (char*)NULL; /* force a new regex search */
                  goto dont_fix_bogon;
                }
              continue;
            }

          switch (ch)
            {
            case NUL:
              return;

            case '\\':
              /*
                Skip escaped newlines.  Otherwise, we have a bogon */
              if (*text != '\n') {
                text--;
                goto fix_the_bogon;
              }

              /*
                Emit the escaped newline and keep scanning for possible junk */
              putc( '\\', stdout );
              putc( '\n', stdout );
              text++;
              break;

            case '/':
              /*
                Skip comments.  Otherwise, we have a bogon */
              if (*text == '*')
                {
                  text--;
                  pz_next = strstr( text+2, "*/" );
                  if (pz_next == (char*)NULL)
                    {
                      putc( '\n', stdout );
                      return;
                    }
                  pz_next += 2;
                  fwrite( text, 1, (pz_next - text), stdout );
                  text = pz_next;
                  break;
                }

              /* FALLTHROUGH */

            default:
              /*
                GOTTA BE A BOGON */
              text--;
              goto fix_the_bogon;
            } /* switch (ch) */
        } /* for (bogon check loop) */

    fix_the_bogon:
      /*
        `text' points to the start of the bogus data */
      for (;;)
        {
          /*
            NOT an escaped newline.  Find the end of line that
            is not preceeded by an escape character:  */
          pz_next = strchr( text, '\n' );
          if (pz_next == (char*)NULL)
            {
              putc( '\n', stdout );
              return;
            }

          if (pz_next[-1] != '\\')
            {
              text = pz_next;
              pz_next = (char*)NULL; /* force a new regex search */
              break;
            }

          /*
            The newline was escaped.  We gotta keep going.  */
          text = pz_next + 1;
        }

    dont_fix_bogon:;
    } /* for (entire file) loop */

  return;
}

/* Scan the input file for all occurrences of text like this:

   #define TIOCCONS _IO(T, 12)

   and change them to read like this:

   #define TIOCCONS _IO('T', 12)

   which is the required syntax per the C standard.  (The definition of
   _IO also has to be tweaked - see below.)  'IO' is actually whatever you
   provide in the STR argument.  */
void
fix_char_macro_uses (text, str)
     const char *text;
     const char *str;
{
  /* This regexp looks for a traditional-syntax #define (# in column 1)
     of an object-like macro.  */
  static const char pat[] =
    "^#[ \t]*define[ \t]+[_A-Za-z][_A-Za-z0-9]*[ \t]+";
  static regex_t re;

  regmatch_t rm[1];
  const char *p, *limit;
  size_t len = strlen (str);

  compile_re (pat, &re, 1, "macro pattern", "fix_char_macro_uses");

  for (p = text;
       regexec (&re, p, 1, rm, 0) == 0;
       p = limit + 1)
    {
      /* p + rm[0].rm_eo is the first character of the macro replacement.
	 Find the end of the macro replacement, and the STR we were
	 sent to look for within the replacement.  */
      p += rm[0].rm_eo;
      limit = p - 1;
      do
	{
	  limit = strchr (limit + 1, '\n');
	  if (!limit)
	    goto done;
	}
      while (limit[-1] == '\\');

      do
	{
	  if (*p == str[0] && !strncmp (p+1, str+1, len-1))
	    goto found;
	}
      while (++p < limit - len);
      /* Hit end of line.  */
      continue;

    found:
      /* Found STR on this line.  If the macro needs fixing,
	 the next few chars will be whitespace or uppercase,
	 then an open paren, then a single letter.  */
      while ((isspace (*p) || isupper (*p)) && p < limit) p++;
      if (*p++ != '(')
	continue;
      if (!isalpha (*p))
	continue;
      if (isalnum (p[1]) || p[1] == '_')
	continue;

      /* Splat all preceding text into the output buffer,
	 quote the character at p, then proceed.  */
      fwrite (text, 1, p - text, stdout);
      putchar ('\'');
      putchar (*p);
      putchar ('\'');
      text = p + 1;
    }
 done:
  fputs (text, stdout);
}

/* Scan the input file for all occurrences of text like this:

   #define _IO(x, y) ('x'<<16+y)

   and change them to read like this:

   #define _IO(x, y) (x<<16+y)

   which is the required syntax per the C standard.  (The uses of _IO
   also have to be tweaked - see above.)  'IO' is actually whatever
   you provide in the STR argument.  */
void
fix_char_macro_defines (text, str)
     const char *text;
     const char *str;
{
  /* This regexp looks for any traditional-syntax #define (# in column 1).  */
  static const char pat[] =
    "^#[ \t]*define[ \t]+";
  static regex_t re;

  regmatch_t rm[1];
  const char *p, *limit;
  size_t len = strlen (str);
  char arg;

  compile_re (pat, &re, 1, "macro pattern", "fix_char_macro_defines");

  for (p = text;
       regexec (&re, p, 1, rm, 0) == 0;
       p = limit + 1)
    {
      /* p + rm[0].rm_eo is the first character of the macro name.
	 Find the end of the macro replacement, and the STR we were
	 sent to look for within the name.  */
      p += rm[0].rm_eo;
      limit = p - 1;
      do
	{
	  limit = strchr (limit + 1, '\n');
	  if (!limit)
	    goto done;
	}
      while (limit[-1] == '\\');

      do
	{
	  if (*p == str[0] && !strncmp (p+1, str+1, len-1))
	    goto found;
	  p++;
	}
      while (isalpha (*p) || isalnum (*p) || *p == '_');
      /* Hit end of macro name without finding the string.  */
      continue;

    found:
      /* Found STR in this macro name.  If the macro needs fixing,
	 there may be a few uppercase letters, then there will be an
	 open paren with _no_ intervening whitespace, and then a
	 single letter.  */
      while (isupper (*p) && p < limit) p++;
      if (*p++ != '(')
	continue;
      if (!isalpha (*p))
	continue;
      if (isalnum (p[1]) || p[1] == '_')
	continue;

      /* The character at P is the one to look for in the following
	 text.  */
      arg = *p;
      p += 2;

      while (p < limit)
	{
	  if (p[-1] == '\'' && p[0] == arg && p[1] == '\'')
	    {
	      /* Remove the quotes from this use of ARG.  */
	      p--;
	      fwrite (text, 1, p - text, stdout);
	      putchar (arg);
	      p += 3;
	      text = p;
	    }
	  else
	    p++;
	}
    }
 done:
  fputs (text, stdout);
}

/* The various prefixes on these macros are handled automatically
   because the fixers don't care where they start matching.  */
FIX_PROC_HEAD( IO_use_fix )
{
  fix_char_macro_uses (text, "IO");
}
FIX_PROC_HEAD( CTRL_use_fix )
{
  fix_char_macro_uses (text, "CTRL");
}

FIX_PROC_HEAD( IO_defn_fix )
{
  fix_char_macro_defines (text, "IO");
}
FIX_PROC_HEAD( CTRL_defn_fix )
{
  fix_char_macro_defines (text, "CTRL");
}


/* Fix for machine name #ifdefs that are not in the namespace reserved
   by the C standard.  They won't be defined if compiling with -ansi,
   and the headers will break.  We go to some trouble to only change
   #ifdefs where the macro is defined by GCC in non-ansi mode; this
   minimizes the number of headers touched.  */

#define SCRATCHSZ 64   /* hopefully long enough */

FIX_PROC_HEAD( machine_name_fix )
{
  regmatch_t match[2];
  char *line, *base, *limit, *p, *q;
  regex_t *label_re, *name_re;
  char scratch[SCRATCHSZ];
  size_t len;

  if (mn_get_regexps (&label_re, &name_re, "machine_name_fix"))
    /* This platform doesn't need this fix.  We can only get here if
       someone is running fixfixes by hand, but let's be polite.  */
    goto done;

  scratch[0] = '_';
  scratch[1] = '_';

  for (base = text;
       regexec (label_re, base, 2, match, 0) == 0;
       base = limit)
    {
      base += match[0].rm_eo;
      /* We're looking at an #if or #ifdef.  Scan forward for the
	 next non-escaped newline.  */
      line = limit = base;
      do
	{
	  limit++;
	  limit = strchr (limit, '\n');
	  if (!limit)
	    goto done;
	}
      while (limit[-1] == '\\');

      /* If the 'name_pat' matches in between base and limit, we have
	 a bogon.  It is not worth the hassle of excluding comments
	 because comments on #if/#ifdef lines are rare, and strings on
	 such lines are illegal.

	 REG_NOTBOL means 'base' is not at the beginning of a line, which
	 shouldn't matter since the name_re has no ^ anchor, but let's
	 be accurate anyway.  */

      for (;;)
	{
	again:
	  if (base == limit)
	    break;

	  if (regexec (name_re, base, 1, match, REG_NOTBOL))
	    goto done;  /* No remaining match in this file */

	  /* Match; is it on the line?  */
	  if (match[0].rm_eo > limit - base)
	    break;

	  p = base + match[0].rm_so;
	  base += match[0].rm_eo;

	  /* One more test: if on the same line we have the same string
	     with the appropriate underscores, then leave it alone.
	     We want exactly two leading and trailing underscores.  */
	  if (*p == '_')
	    {
	      len = base - p - ((*base == '_') ? 2 : 1);
	      q = p + 1;
	    }
	  else
	    {
	      len = base - p - ((*base == '_') ? 1 : 0);
	      q = p;
	    }
	  if (len + 4 > SCRATCHSZ)
	    abort ();
	  memcpy (&scratch[2], q, len);
	  len += 2;
	  scratch[len++] = '_';
	  scratch[len++] = '_';

	  for (q = line; q <= limit - len; q++)
	    if (*q == '_' && !strncmp (q, scratch, len))
	      goto again;
	  
	  fwrite (text, 1, p - text, stdout);
	  fwrite (scratch, 1, len, stdout);

	  text = base;
	}
    }
 done:
  fputs (text, stdout);
}


/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

     test for fix selector

     THIS IS THE ONLY EXPORTED ROUTINE

*/
void
apply_fix( fixname, filname )
  const char* fixname;
  const char* filname;
{
#define _FT_(n,p) { n, p },
  static fix_entry_t fix_table[] = { FIXUP_TABLE { NULL, NULL }};
#undef _FT_
#define FIX_TABLE_CT ((sizeof(fix_table)/sizeof(fix_table[0]))-1)

  char* buf;
  int ct = FIX_TABLE_CT;
  fix_entry_t* pfe = fix_table;

  for (;;)
    {
      if (strcmp (pfe->fix_name, fixname) == 0)
        break;
      if (--ct <= 0)
        {
          fprintf (stderr, "fixincludes error:  the `%s' fix is unknown\n",
                   fixname );
          exit (3);
        }
      pfe++;
    }

  buf = load_file_data (stdin);
  (*pfe->fix_proc)( filname, buf );
}

#ifdef MAIN

/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

     MAIN ROUTINE

     This file is both included in fixincl.c and compiled as a separate
     program for use by the inclhack.sh script.

*/

int
main( argc, argv )
  int argc;
  char** argv;
{
  if (argc != 3)
    apply_fix ("No test name provided", NULL, NULL, 0 );

  apply_fix (argv[2], argv[1]);
  return 0;
}

#endif
