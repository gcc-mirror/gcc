
/*

   Test to see if a particular fix should be applied to a header file.

   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

= = = = = = = = = = = = = = = = = = = = = = = = =

NOTE TO DEVELOPERS

The routines you write here must work closely with fixincl.c.

Here are the rules:

1.  Every test procedure name must be suffixed with "_fix".
    These routines will be referenced from inclhack.def, sans the suffix.

2.  Use the "FIX_PROC_HEAD()" macro _with_ the "_fix" suffix
    (I cannot use the ## magic from ANSI C) for defining your entry point.

3.  Put your test name into the FIXUP_TABLE.

4.  Do not read anything from stdin.  It is closed.

5.  Write to stderr only in the event of a reportable error
    In such an event, call "exit (EXIT_FAILURE)".

6.  You have access to the fixDescList entry for the fix in question.
    This may be useful, for example, if there are interesting strings
    or pre-compiled regular expressions stored there.

    It is also possible to access fix descriptions by using the
    index of a known fix, "my_fix_name" for example:

        tFixDesc*  p_desc  = fixDescList + MY_FIX_NAME_FIXIDX;
        tTestDesc* p_tlist = p_desc->p_test_desc;

        regexec (p_tlist->p_test_regex, ...)

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
#define    GTYPE_SE_CT 1

tSCC zNeedsArg[] = "fixincl error:  `%s' needs %s argument (c_fix_arg[%d])\n";

typedef struct {
    const char*  fix_name;
    void (*fix_proc)();
} fix_entry_t;

#define FIXUP_TABLE \
  _FT_( "char_macro_def",   char_macro_def_fix ) \
  _FT_( "char_macro_use",   char_macro_use_fix ) \
  _FT_( "format",           format_fix )         \
  _FT_( "machine_name",     machine_name_fix )   \
  _FT_( "wrap",             wrap_fix )           \
  _FT_( "gnu_type",         gnu_type_fix )


#define FIX_PROC_HEAD( fix ) \
static void fix ( filname, text, p_fixd ) \
    const char* filname; \
    const char* text; \
    tFixDesc* p_fixd;


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


/*
 *  Emit the GNU standard type wrapped up in such a way that
 *  this thing can be encountered countless times during a compile
 *  and not cause even a warning.
 */
static const char*
emit_gnu_type ( text, rm )
  const char* text;
  regmatch_t* rm;
{
  extern t_gnu_type_map gnu_type_map[];
  extern int gnu_type_map_ct;

  const char*     pzt  = text + rm[GTYPE_SE_CT].rm_so;
  t_gnu_type_map* p_tm = gnu_type_map;
  int   ct = gnu_type_map_ct;

  fwrite (text, rm[0].rm_so, 1, stdout);
  text += rm[0].rm_eo;

  for (;;)
    {
      if (strncmp (pzt, p_tm->pz_type, p_tm->type_name_len) == 0)
        break;

#ifdef DEBUG
      if (--ct <= 0)
        return (const char*)NULL;
#else
      if (--ct <= 0)
        return text;
#endif
      p_tm++;
    }

  /*
   *  Now print out the reformed typedef
   */
  printf ("\
#ifndef __%s_TYPE__\n\
#define __%s_TYPE__ %s\n\
#endif\n",
          p_tm->pz_TYPE, p_tm->pz_TYPE, p_tm->pz_gtype );

  printf ("\
#if !defined(_GCC_%s_T)%s\n\
#define _GCC_%s_T\n\
typedef __%s_TYPE__ %s_t;\n\
#endif\n",
          p_tm->pz_TYPE, p_tm->pz_cxx_guard,
          p_tm->pz_TYPE, p_tm->pz_TYPE, p_tm->pz_type);

  return text;
}


/*
 *  Copy the `format' string to std out, replacing `%n' expressions
 *  with the matched text from a regular expression evaluation.
 *  Doubled '%' characters will be replaced with a single copy.
 *  '%' characters in other contexts and all other characters are
 *  copied out verbatim.
 */
static void
format_write (format, text, av)
     tCC* format;
     tCC* text;
     regmatch_t av[];
{
  int c;

  while ((c = (unsigned)*(format++)) != NUL) {

    if (c != '%')
      {
        putchar(c);
        continue;
      }

    c = (unsigned)*(format++);

    /*
     *  IF the character following a '%' is not a digit,
     *  THEN we will always emit a '%' and we may or may
     *  not emit the following character.  We will end on
     *  a NUL and we will emit only one of a pair of '%'.
     */
    if (! isdigit( c ))
      {
        putchar( '%' );
        switch (c) {
        case NUL:
          return;
        case '%':
          break;
        default:
          putchar(c);
        }
      }

    /*
     *  Emit the matched subexpression numbered 'c'.
     *  IF, of course, there was such a match...
     */
    else {
      regmatch_t*  pRM = av + (c - (unsigned)'0');
      size_t len;

      if (pRM->rm_so < 0)
        continue;

      len = pRM->rm_eo - pRM->rm_so;
      if (len > 0)
        fwrite(text + pRM->rm_so, len, 1, stdout);
    }
  }
}


/*
 *  Search for multiple copies of a regular expression.  Each block
 *  of matched text is replaced with the format string, as described
 *  above in `format_write'.
 */
FIX_PROC_HEAD( format_fix )
{
  tCC*  pz_pat = p_fixd->patch_args[2];
  tCC*  pz_fmt = p_fixd->patch_args[1];
  const char *p;
  regex_t re;
  regmatch_t rm[10];

  /*
   *  We must have a format
   */
  if (pz_fmt == (tCC*)NULL)
    {
      fprintf( stderr, zNeedsArg, p_fixd->fix_name, "replacement format", 0 );
      exit (EXIT_BROKEN);
    }

  /*
   *  IF we don't have a search text, then go find the first
   *  regular expression among the tests.
   */
  if (pz_pat == (tCC*)NULL)
    {
      tTestDesc* pTD = p_fixd->p_test_desc;
      int        ct  = p_fixd->test_ct;
      for (;;)
        {
          if (ct-- <= 0)
            {
              fprintf( stderr, zNeedsArg, p_fixd->fix_name, "search text", 1 );
              exit (EXIT_BROKEN);
            }

          if (pTD->type == TT_EGREP)
            {
              pz_pat = pTD->pz_test_text;
              break;
            }

          pTD++;
        }
    }

  /*
   *  Replace every copy of the text we find
   */
  compile_re (pz_pat, &re, 1, "format search-text", "format_fix" );
  while (regexec (&re, text, 10, rm, 0) == 0)
    {
      char* apz[10];
      int   i;

      fwrite( text, rm[0].rm_so, 1, stdout );
      format_write( pz_fmt, text, rm );
      text += rm[0].rm_eo;
    }

  /*
   *  Dump out the rest of the file
   */
  fputs (text, stdout);
}


/* Scan the input file for all occurrences of text like this:

   #define TIOCCONS _IO(T, 12)

   and change them to read like this:

   #define TIOCCONS _IO('T', 12)

   which is the required syntax per the C standard.  (The definition of
   _IO also has to be tweaked - see below.)  'IO' is actually whatever you
   provide as the `c_fix_arg' argument.  */

FIX_PROC_HEAD( char_macro_use_fix )
{
  /* This regexp looks for a traditional-syntax #define (# in column 1)
     of an object-like macro.  */
  static const char pat[] =
    "^#[ \t]*define[ \t]+[_A-Za-z][_A-Za-z0-9]*[ \t]+";
  static regex_t re;

  const char* str = p_fixd->patch_args[1];
  regmatch_t rm[1];
  const char *p, *limit;
  size_t len;

  if (str == NULL)
    {
      fprintf (stderr, zNeedsArg, p_fixd->fix_name, "ioctl type", 0);
      exit (EXIT_BROKEN);
    }

  len = strlen (str);
  compile_re (pat, &re, 1, "macro pattern", "char_macro_use_fix");

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

   #define xxxIOxx(x, y) (....'x'<<16....)

   and change them to read like this:

   #define xxxIOxx(x, y) (....x<<16....)

   which is the required syntax per the C standard.  (The uses of _IO
   also has to be tweaked - see above.)  'IO' is actually whatever
   you provide as the `c_fix_arg' argument.  */
FIX_PROC_HEAD( char_macro_def_fix )
{
  /* This regexp looks for any traditional-syntax #define (# in column 1).  */
  static const char pat[] =
    "^#[ \t]*define[ \t]+";
  static regex_t re;

  const char* str = p_fixd->patch_args[1];
  regmatch_t rm[1];
  const char *p, *limit;
  char arg;
  size_t len;

  if (str == NULL)
    {
      fprintf (stderr, zNeedsArg, p_fixd->fix_name, "ioctl type", 0);
      exit (EXIT_BROKEN);
    }

  len = strlen (str);
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

/* Fix for machine name #ifdefs that are not in the namespace reserved
   by the C standard.  They won't be defined if compiling with -ansi,
   and the headers will break.  We go to some trouble to only change
   #ifdefs where the macro is defined by GCC in non-ansi mode; this
   minimizes the number of headers touched.  */

#define SCRATCHSZ 64   /* hopefully long enough */

FIX_PROC_HEAD( machine_name_fix )
{
#ifndef MN_NAME_PAT
  fputs( "The target machine has no needed machine name fixes\n", stderr );
#else
  regmatch_t match[2];
  const char *line, *base, *limit, *p, *q;
  regex_t *label_re, *name_re;
  char scratch[SCRATCHSZ];
  size_t len;

  mn_get_regexps (&label_re, &name_re, "machine_name_fix");

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
#endif
  fputs (text, stdout);
}


FIX_PROC_HEAD( wrap_fix )
{
  char   z_fixname[ 64 ];
  tCC*   pz_src  = p_fixd->fix_name;
  tCC*   pz_name = z_fixname;
  char*  pz_dst  = z_fixname;
  size_t len     = 0;

  for (;;) {
    char ch = *(pz_src++);

    if (islower(ch))
      *(pz_dst++) = toupper( ch );

    else if (isalnum( ch ))
      *(pz_dst++) = ch;

    else if (ch == NUL) {
      *(pz_dst++) = ch;
      break;
    }
    else
      *(pz_dst++) = '_';

    if (++len >= sizeof( z_fixname )) {
      void* p = xmalloc( len + strlen( pz_src ) + 1 );
      memcpy( p, (void*)z_fixname, len );
      pz_name = (tCC*)p;
      pz_dst  = (char*)pz_name + len;
    }
  }

  printf( "#ifndef FIXINC_%s_CHECK\n", pz_name );
  printf( "#define FIXINC_%s_CHECK 1\n\n", pz_name );

  if (p_fixd->patch_args[1] == (tCC*)NULL)
    fputs( text, stdout );

  else {
    fputs( p_fixd->patch_args[1], stdout );
    fputs( text, stdout );
    if (p_fixd->patch_args[2] != (tCC*)NULL)
      fputs( p_fixd->patch_args[2], stdout );
  }

  printf( "\n#endif  /* FIXINC_%s_CHECK */\n", pz_name );
  if (pz_name != z_fixname)
    free( (void*)pz_name );
}


/*
 *  Search for multiple copies of a regular expression.  Each block
 *  of matched text is replaced with the format string, as described
 *  above in `format_write'.
 */
FIX_PROC_HEAD( gnu_type_fix )
{
  const char* pz_pat;
  regex_t    re;
  regmatch_t rm[GTYPE_SE_CT+1];

  {
    tTestDesc* pTD = p_fixd->p_test_desc;
    int        ct  = p_fixd->test_ct;
    for (;;)
      {
        if (ct-- <= 0)
          {
            fprintf (stderr, zNeedsArg, p_fixd->fix_name, "search text", 1);
            exit (EXIT_BROKEN);
          }

        if (pTD->type == TT_EGREP)
          {
            pz_pat = pTD->pz_test_text;
            break;
          }

        pTD++;
      }
  }

  compile_re (pz_pat, &re, 1, "gnu type typedef", "gnu_type_fix");

  while (regexec (&re, text, GTYPE_SE_CT+1, rm, 0) == 0)
    {
#ifndef DEBUG
      text = emit_gnu_type (text, rm);
#else
      tSCC z_mismatch[] = "``%s'' mismatched:\n";

      /*
       *  Make sure we matched *all* subexpressions
       */
      if (rm[GTYPE_SE_CT].rm_so == -1)
        {
          int i;

          fprintf (stderr, z_mismatch, pz_pat);

          for (i=0; i <= GTYPE_SE_CT; i++)
            {
              if (rm[i].rm_so != -1)
                {
                  fprintf( stderr, "%4d:  ``", i );
                  fwrite( text + rm[i].rm_so, rm[i].rm_eo - rm[i].rm_so,
                          1, stderr );
                  fputs( "''\n", stderr );
                }
              else
                {
                  fprintf( stderr, "%4d:  BROKEN\n", i );
                }
            }
          exit (EXIT_BROKEN);
        }

      text = emit_gnu_type (text, rm);
      if (text == NULL)
        {
          fprintf (stderr, z_mismatch, pz_pat);
          exit (EXIT_BROKEN);
        }
#endif
    }

  /*
   *  Dump out the rest of the file
   */
  fputs (text, stdout);
}


/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

     test for fix selector

     THIS IS THE ONLY EXPORTED ROUTINE

*/
void
apply_fix( p_fixd, filname )
  tFixDesc* p_fixd;
  tCC* filname;
{
#define _FT_(n,p) { n, p },
  static fix_entry_t fix_table[] = { FIXUP_TABLE { NULL, NULL }};
#undef _FT_
#define FIX_TABLE_CT ((sizeof(fix_table)/sizeof(fix_table[0]))-1)

  tCC* fixname = p_fixd->patch_args[0];
  char* buf;
  int ct = FIX_TABLE_CT;
  fix_entry_t* pfe = fix_table;

  for (;;)
    {
      if (strcmp (pfe->fix_name, fixname) == 0)
        break;
      if (--ct <= 0)
        {
          fprintf (stderr, "fixincl error:  the `%s' fix is unknown\n",
                   fixname );
          exit (EXIT_BROKEN);
        }
      pfe++;
    }

  buf = load_file_data (stdin);
  (*pfe->fix_proc)( filname, buf, p_fixd );
}
