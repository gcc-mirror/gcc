/* fini.c
   Copyright (C) 1995 Free Software Foundation, Inc.
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

#define USE_HCONFIG

#include "proj.h"
#include "malloc.h"

#undef MAXNAMELEN
#define MAXNAMELEN 100

typedef struct _name_ *name;

struct _name_
  {
    name next;
    name previous;
    name next_alpha;
    name previous_alpha;
    int namelen;
    int kwlen;
    char kwname[MAXNAMELEN];
    char name_uc[MAXNAMELEN];
    char name_lc[MAXNAMELEN];
    char name_ic[MAXNAMELEN];
  };

struct _name_root_
  {
    name first;
    name last;
  };

struct _name_alpha_
  {
    name ign1;
    name ign2;
    name first;
    name last;
  };

static FILE *in;
static FILE *out;
static char prefix[32];
static char postfix[32];
static char storage[32];
static const char *xspaces[]
=
{
  "",				/* 0 */
  " ",				/* 1 */
  "  ",				/* 2 */
  "   ",			/* 3 */
  "    ",			/* 4 */
  "     ",			/* 5 */
  "      ",			/* 6 */
  "       ",			/* 7 */
  "\t",				/* 8 */
  "\t ",			/* 9 */
  "\t  ",			/* 10 */
  "\t   ",			/* 11 */
  "\t    ",			/* 12 */
  "\t     ",			/* 13 */
  "\t      ",			/* 14 */
  "\t       ",			/* 15 */
  "\t\t",			/* 16 */
  "\t\t ",			/* 17 */
  "\t\t  ",			/* 18 */
  "\t\t   ",			/* 19 */
  "\t\t    ",			/* 20 */
  "\t\t     ",			/* 21 */
  "\t\t      ",			/* 22 */
  "\t\t       ",		/* 23 */
  "\t\t\t",			/* 24 */
  "\t\t\t ",			/* 25 */
  "\t\t\t  ",			/* 26 */
  "\t\t\t   ",			/* 27 */
  "\t\t\t    ",			/* 28 */
  "\t\t\t     ",		/* 29 */
  "\t\t\t      ",		/* 30 */
  "\t\t\t       ",		/* 31 */
  "\t\t\t\t",			/* 32 */
  "\t\t\t\t ",			/* 33 */
  "\t\t\t\t  ",			/* 34 */
  "\t\t\t\t   ",		/* 35 */
  "\t\t\t\t    ",		/* 36 */
  "\t\t\t\t     ",		/* 37 */
  "\t\t\t\t      ",		/* 38 */
  "\t\t\t\t       ",		/* 39 */
  "\t\t\t\t\t",			/* 40 */
  "\t\t\t\t\t ",		/* 41 */
  "\t\t\t\t\t  ",		/* 42 */
  "\t\t\t\t\t   ",		/* 43 */
  "\t\t\t\t\t    ",		/* 44 */
  "\t\t\t\t\t     ",		/* 45 */
  "\t\t\t\t\t      ",		/* 46 */
  "\t\t\t\t\t       ",		/* 47 */
  "\t\t\t\t\t\t",		/* 48 */
  "\t\t\t\t\t\t ",		/* 49 */
  "\t\t\t\t\t\t  ",		/* 50 */
  "\t\t\t\t\t\t   ",		/* 51 */
  "\t\t\t\t\t\t    ",		/* 52 */
  "\t\t\t\t\t\t     ",		/* 53 */
  "\t\t\t\t\t\t      ",		/* 54 */
  "\t\t\t\t\t\t       ",	/* 55 */
  "\t\t\t\t\t\t\t",		/* 56 */
  "\t\t\t\t\t\t\t ",		/* 57 */
  "\t\t\t\t\t\t\t  ",		/* 58 */
  "\t\t\t\t\t\t\t   ",		/* 59 */
  "\t\t\t\t\t\t\t    ",		/* 60 */
  "\t\t\t\t\t\t\t     ",	/* 61 */
  "\t\t\t\t\t\t\t      ",	/* 62 */
  "\t\t\t\t\t\t\t       ",	/* 63 */
  "\t\t\t\t\t\t\t\t",		/* 64 */
  "\t\t\t\t\t\t\t\t ",		/* 65 */
  "\t\t\t\t\t\t\t\t  ",		/* 66 */
  "\t\t\t\t\t\t\t\t   ",	/* 67 */
  "\t\t\t\t\t\t\t\t    ",	/* 68 */
  "\t\t\t\t\t\t\t\t     ",	/* 69 */
  "\t\t\t\t\t\t\t\t      ",	/* 70 */
  "\t\t\t\t\t\t\t\t       ",	/* 71 */
  "\t\t\t\t\t\t\t\t\t",		/* 72 */
  "\t\t\t\t\t\t\t\t\t ",	/* 73 */
  "\t\t\t\t\t\t\t\t\t  ",	/* 74 */
  "\t\t\t\t\t\t\t\t\t   ",	/* 75 */
  "\t\t\t\t\t\t\t\t\t    ",	/* 76 */
  "\t\t\t\t\t\t\t\t\t     ",	/* 77 */
  "\t\t\t\t\t\t\t\t\t      ",	/* 78 */
  "\t\t\t\t\t\t\t\t\t       ",	/* 79 */
  "\t\t\t\t\t\t\t\t\t\t",	/* 80 */
  "\t\t\t\t\t\t\t\t\t\t ",	/* 81 */
  "\t\t\t\t\t\t\t\t\t\t  ",	/* 82 */
  "\t\t\t\t\t\t\t\t\t\t   ",	/* 83 */
  "\t\t\t\t\t\t\t\t\t\t    ",	/* 84 */
  "\t\t\t\t\t\t\t\t\t\t     ",	/* 85 */
  "\t\t\t\t\t\t\t\t\t\t      ",	/* 86 */
  "\t\t\t\t\t\t\t\t\t\t       ",/* 87 */
  "\t\t\t\t\t\t\t\t\t\t\t",	/* 88 */
  "\t\t\t\t\t\t\t\t\t\t\t ",	/* 89 */
  "\t\t\t\t\t\t\t\t\t\t\t  ",	/* 90 */
  "\t\t\t\t\t\t\t\t\t\t\t   ",	/* 91 */
  "\t\t\t\t\t\t\t\t\t\t\t    ",	/* 92 */
  "\t\t\t\t\t\t\t\t\t\t\t     ",/* 93 */
  "\t\t\t\t\t\t\t\t\t\t\t      ",	/* 94 */
  "\t\t\t\t\t\t\t\t\t\t\t       ",	/* 95 */
  "\t\t\t\t\t\t\t\t\t\t\t\t",	/* 96 */
  "\t\t\t\t\t\t\t\t\t\t\t\t ",	/* 97 */
  "\t\t\t\t\t\t\t\t\t\t\t\t  ",	/* 98 */
  "\t\t\t\t\t\t\t\t\t\t\t\t   ",/* 99 */
  "\t\t\t\t\t\t\t\t\t\t\t\t    ",	/* 100 */
  "\t\t\t\t\t\t\t\t\t\t\t\t     ",	/* 101 */
  "\t\t\t\t\t\t\t\t\t\t\t\t      ",	/* 102 */
  "\t\t\t\t\t\t\t\t\t\t\t\t       ",	/* 103 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t",	/* 104 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t ",/* 105 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t  ",	/* 106 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t   ",	/* 107 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t    ",	/* 108 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t     ",	/* 109 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t      ",	/* 110 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t       ",	/* 111 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t",	/* 112 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t ",	/* 113 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t  ",	/* 114 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t   ",	/* 115 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t    ",	/* 116 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t     ",	/* 117 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t      ",	/* 118 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t       ",	/* 119 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t",	/* 120 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t ",	/* 121 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t  ",	/* 122 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t   ",	/* 123 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t    ",	/* 124 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t     ",	/* 125 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t      ",	/* 126 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t       ",	/* 127 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t",	/* 128 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t ",	/* 129 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t  ",	/* 130 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t   ",	/* 131 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t    ",	/* 132 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t     ",	/* 133 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t      ",	/* 134 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t       ",	/* 135 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t",	/* 136 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t ",	/* 137 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t  ",	/* 138 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t   ",	/* 139 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t    ",	/* 140 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t     ",	/* 141 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t      ",	/* 142 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t       ",	/* 143 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t",	/* 144 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t ",	/* 145 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t  ",	/* 146 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t   ",	/* 147 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t    ",	/* 148 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t     ",	/* 149 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t      ",	/* 150 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t       ",	/* 151 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t",	/* 152 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t ",	/* 153 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t  ",	/* 154 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t   ",	/* 155 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t    ",	/* 156 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t     ",	/* 157 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t      ",	/* 158 */
  "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t       ",	/* 159 */
};

void testname (bool nested, int indent, name first, name last);
void testnames (bool nested, int indent, int len, name first, name last);

int
main (int argc, char **argv)
{
  char buf[MAXNAMELEN];
  char last_buf[MAXNAMELEN] = "";
  char kwname[MAXNAMELEN];
  char routine[32];
  char type[32];
  int i;
  int count;
  int len;
  struct _name_root_ names[200];
  struct _name_alpha_ names_alpha;
  name n;
  name newname;
  char *input_name;
  char *output_name;
  char *include_name;
  FILE *incl;
  int fixlengths;
  int total_length;
  int do_name;			/* TRUE if token may be NAME. */
  int do_names;			/* TRUE if token may be NAMES. */
  int cc;
  bool do_exit = FALSE;

  for (i = 0; ((size_t) i) < ARRAY_SIZE (names); ++i)
    {				/* Initialize length/name ordered list roots. */
      names[i].first = (name) &names[i];
      names[i].last = (name) &names[i];
    }
  names_alpha.first = (name) &names_alpha;	/* Initialize name order. */
  names_alpha.last = (name) &names_alpha;

  if (argc != 4)
    {
      fprintf (stderr, "Command form: fini input output-code output-include\n");
      exit (1);
    }

  input_name = argv[1];
  output_name = argv[2];
  include_name = argv[3];

  in = fopen (input_name, "r");
  if (in == NULL)
    {
      fprintf (stderr, "Cannot open \"%s\"\n", input_name);
      exit (1);
    }
  out = fopen (output_name, "w");
  if (out == NULL)
    {
      fclose (in);
      fprintf (stderr, "Cannot open \"%s\"\n", output_name);
      exit (1);
    }
  incl = fopen (include_name, "w");
  if (incl == NULL)
    {
      fclose (in);
      fprintf (stderr, "Cannot open \"%s\"\n", include_name);
      exit (1);
    }

  /* Get past the initial block-style comment (man, this parsing code is just
     _so_ lame, but I'm too lazy to improve it).  */

  for (;;)
    {
      cc = getc (in);
      if (cc == '{')
	{
	  while (((cc = getc (in)) != '}') && (cc != EOF))
	    ;
	}
      else if (cc != EOF)
	{
	  while (((cc = getc (in)) != EOF) && (! ISALNUM (cc)))
	    ;
	  ungetc (cc, in);
	  break;
	}
      else
	{
	  assert ("EOF too soon!" == NULL);
	  exit (1);
	}
    }

  fscanf (in, "%s %s %s %s %s %d %d", prefix, postfix, storage, type, routine,
	  &do_name, &do_names);

  if (storage[0] == '\0')
    storage[1] = '\0';
  else
    /* Assume string is quoted somehow, replace ending quote with space. */
    {
      if (storage[2] == '\0')
	storage[1] = '\0';
      else
	storage[strlen (storage) - 1] = ' ';
    }

  if (postfix[0] == '\0')
    postfix[1] = '\0';
  else				/* Assume string is quoted somehow, strip off
				   ending quote. */
    postfix[strlen (postfix) - 1] = '\0';

  for (i = 1; storage[i] != '\0'; ++i)
    storage[i - 1] = storage[i];
  storage[i - 1] = '\0';

  for (i = 1; postfix[i] != '\0'; ++i)
    postfix[i - 1] = postfix[i];
  postfix[i - 1] = '\0';

  fixlengths = strlen (prefix) + strlen (postfix);

  while (TRUE)
    {
      count = fscanf (in, "%s %s", buf, kwname);
      if (count == EOF)
	break;
      len = strlen (buf);
      if (len == 0)
	continue;		/* Skip empty lines. */
      if (buf[0] == ';')
	continue;		/* Skip commented-out lines. */
      for (i = strlen (buf) - 1; i > 0; --i)
	cc = buf[i];

      /* Make new name object to store name and its keyword. */

      newname = (name) malloc (sizeof (*newname));
      newname->namelen = strlen (buf);
      newname->kwlen = strlen (kwname);
      total_length = newname->kwlen + fixlengths;
      if (total_length >= 32)	/* Else resulting keyword name too long. */
	{
	  fprintf (stderr, "%s: %s%s%s is 31+%d chars long\n", input_name,
		   prefix, kwname, postfix, total_length - 31);
	  do_exit = TRUE;
	}
      strcpy (newname->kwname, kwname);
      for (i = 0; i < newname->namelen; ++i)
	{
	  cc = buf[i];
	  if (ISALPHA (cc))
	    {
	      newname->name_uc[i] = toupper (cc);
	      newname->name_lc[i] = tolower (cc);
	      newname->name_ic[i] = cc;
	    }
	  else
	    newname->name_uc[i] = newname->name_lc[i] = newname->name_ic[i]
	      = cc;
	}
      newname->name_uc[i] = newname->name_lc[i] = newname->name_ic[i] = '\0';

      /* Warn user if names aren't alphabetically ordered. */

      if ((last_buf[0] != '\0')
	  && (strcmp (last_buf, newname->name_uc) >= 0))
	{
	  fprintf (stderr, "%s: \"%s\" precedes \"%s\"\n", input_name,
		   last_buf, newname->name_uc);
	  do_exit = TRUE;
	}
      strcpy (last_buf, newname->name_uc);

      /* Append name to end of alpha-sorted list (assumes names entered in
	 alpha order wrt name, not kwname, even though kwname is output from
	 this list). */

      n = names_alpha.last;
      newname->next_alpha = n->next_alpha;
      newname->previous_alpha = n;
      n->next_alpha->previous_alpha = newname;
      n->next_alpha = newname;

      /* Insert name in appropriate length/name ordered list. */

      n = (name) &names[len];
      while ((n->next != (name) &names[len])
	     && (strcmp (buf, n->next->name_uc) > 0))
	n = n->next;
      if (strcmp (buf, n->next->name_uc) == 0)
	{
	  fprintf (stderr, "%s: extraneous \"%s\"\n", input_name, buf);
	  do_exit = TRUE;
	}
      newname->next = n->next;
      newname->previous = n;
      n->next->previous = newname;
      n->next = newname;
    }

#if 0
  for (len = 0; len < ARRAY_SIZE (name); ++len)
    {
      if (names[len].first == (name) &names[len])
	continue;
      printf ("Length %d:\n", len);
      for (n = names[len].first; n != (name) &names[len]; n = n->next)
	printf ("    %s %s %s\n", n->name_uc, n->name_lc, n->name_ic);
    }
#endif

  if (do_exit)
    exit (1);

  /* First output the #include file. */

  for (n = names_alpha.first; n != (name) &names_alpha; n = n->next_alpha)
    {
      fprintf (incl, "#define %sl%s%s %d\n", prefix, n->kwname, postfix,
	       n->namelen);
    }

  fprintf (incl,
	   "\
\n\
enum %s_\n\
{\n\
%sNone%s,\n\
",
	   type, prefix, postfix);

  for (n = names_alpha.first; n != (name) &names_alpha; n = n->next_alpha)
    {
      fprintf (incl,
	       "\
%s%s%s,\n\
",
	       prefix, n->kwname, postfix);
    }

  fprintf (incl,
	   "\
%s%s\n\
};\n\
typedef enum %s_ %s;\n\
",
	   prefix, postfix, type, type);

  /* Now output the C program. */

  fprintf (out,
	   "\
%s%s\n\
%s (ffelexToken t)\n\
%c\n\
  char *p;\n\
  int c;\n\
\n\
  p = ffelex_token_text (t);\n\
\n\
",
	   storage, type, routine, '{');

  if (do_name)
    {
      if (do_names)
	fprintf (out,
		 "\
  if (ffelex_token_type (t) == FFELEX_typeNAME)\n\
    {\n\
      switch (ffelex_token_length (t))\n\
\t{\n\
"
	  );
      else
	fprintf (out,
		 "\
  assert (ffelex_token_type (t) == FFELEX_typeNAME);\n\
\n\
  switch (ffelex_token_length (t))\n\
    {\n\
"
	  );

/* Now output the length as a case, followed by the binary search within that length.  */

      for (len = 0; ((size_t) len) < ARRAY_SIZE (names); ++len)
	{
	  if (names[len].first != (name) &names[len])
	    {
	      if (do_names)
		fprintf (out,
			 "\
\tcase %d:\n\
",
			 len);
	      else
		fprintf (out,
			 "\
    case %d:\n\
",
			 len);
	      testname (FALSE, do_names ? 10 : 6, names[len].first, names[len].last);
	      if (do_names)
		fprintf (out,
			 "\
\t  break;\n\
"
		  );
	      else
		fprintf (out,
			 "\
      break;\n\
"
		  );
	    }
	}

      if (do_names)
	fprintf (out,
		 "\
\t}\n\
      return %sNone%s;\n\
    }\n\
\n\
",
		 prefix, postfix);
      else
	fprintf (out,
		 "\
    }\n\
\n\
  return %sNone%s;\n\
}\n\
",
		 prefix, postfix);
    }

  if (do_names)
    {
      fputs ("\
  assert (ffelex_token_type (t) == FFELEX_typeNAMES);\n\
\n\
  switch (ffelex_token_length (t))\n\
    {\n\
    default:\n\
",
	     out);

      /* Find greatest non-empty length list. */

      for (len = ARRAY_SIZE (names) - 1;
	   names[len].first == (name) &names[len];
	   --len)
	;

/* Now output the length as a case, followed by the binary search within that length. */

      if (len > 0)
	{
	  for (; len != 0; --len)
	    {
	      fprintf (out,
		       "\
    case %d:\n\
",
		       len);
	      if (names[len].first != (name) &names[len])
		testnames (FALSE, 6, len, names[len].first, names[len].last);
	    }
	  if (names[1].first == (name) &names[1])
	    fprintf (out,
		     "\
      ;\n\
"
	      );		/* Need empty statement after an empty case
				   1:  */
	}

      fprintf (out,
	       "\
    }\n\
\n\
  return %sNone%s;\n\
}\n\
",
	       prefix, postfix);
    }

  if (out != stdout)
    fclose (out);
  if (incl != stdout)
    fclose (incl);
  if (in != stdin)
    fclose (in);
  exit (0);
}

void
testname (bool nested, int indent, name first, name last)
{
  name n;
  name nhalf;
  int num;
  int numhalf;

  assert (!nested || indent >= 2);
  assert (((size_t) indent) + 4 < ARRAY_SIZE (xspaces));

  num = 0;
  numhalf = 0;
  for (n = first, nhalf = first; n != last->next; n = n->next)
    {
      if ((++num & 1) == 0)
	{
	  nhalf = nhalf->next;
	  ++numhalf;
	}
    }

  if (nested)
    fprintf (out,
	     "\
%s{\n\
",
	     xspaces[indent - 2]);

  fprintf (out,
	   "\
%sif ((c = ffesrc_strcmp_2c (ffe_case_match (), p, \"%s\", \"%s\", \"%s\")) == 0)\n\
%sreturn %s%s%s;\n\
",
	   xspaces[indent], nhalf->name_uc, nhalf->name_lc, nhalf->name_ic,
	   xspaces[indent + 2], prefix, nhalf->kwname, postfix);

  if (num != 1)
    {
      fprintf (out,
	       "\
%selse if (c < 0)\n\
",
	       xspaces[indent]);

      if (numhalf == 0)
	fprintf (out,
		 "\
%s;\n\
",
		 xspaces[indent + 2]);
      else
	testname (TRUE, indent + 4, first, nhalf->previous);

      if (num - numhalf > 1)
	{
	  fprintf (out,
		   "\
%selse\n\
",
		   xspaces[indent]);

	  testname (TRUE, indent + 4, nhalf->next, last);
	}
    }

  if (nested)
    fprintf (out,
	     "\
%s}\n\
",
	     xspaces[indent - 2]);
}

void
testnames (bool nested, int indent, int len, name first, name last)
{
  name n;
  name nhalf;
  int num;
  int numhalf;

  assert (!nested || indent >= 2);
  assert (((size_t) indent) + 4 < ARRAY_SIZE (xspaces));

  num = 0;
  numhalf = 0;
  for (n = first, nhalf = first; n != last->next; n = n->next)
    {
      if ((++num & 1) == 0)
	{
	  nhalf = nhalf->next;
	  ++numhalf;
	}
    }

  if (nested)
    fprintf (out,
	     "\
%s{\n\
",
	     xspaces[indent - 2]);

  fprintf (out,
	   "\
%sif ((c = ffesrc_strncmp_2c (ffe_case_match (), p, \"%s\", \"%s\", \"%s\", %d)) == 0)\n\
%sreturn %s%s%s;\n\
",
	   xspaces[indent], nhalf->name_uc, nhalf->name_lc, nhalf->name_ic,
	   len, xspaces[indent + 2], prefix, nhalf->kwname, postfix);

  if (num != 1)
    {
      fprintf (out,
	       "\
%selse if (c < 0)\n\
",
	       xspaces[indent]);

      if (numhalf == 0)
	fprintf (out,
		 "\
%s;\n\
",
		 xspaces[indent + 2]);
      else
	testnames (TRUE, indent + 4, len, first, nhalf->previous);

      if (num - numhalf > 1)
	{
	  fprintf (out,
		   "\
%selse\n\
",
		   xspaces[indent]);

	  testnames (TRUE, indent + 4, len, nhalf->next, last);
	}
    }

  if (nested)
    fprintf (out,
	     "\
%s}\n\
",
	     xspaces[indent - 2]);
}
