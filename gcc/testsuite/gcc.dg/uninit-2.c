/* Spurious uninitialized variable warnings, case 2.
   Taken from cpphash.c (macroexpand) */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

struct definition
{
  int nargs;
  int rest_args;
};

struct cpp_reader;

enum cpp_token
{
  CPP_EOF, CPP_POP, CPP_COMMA, CPP_RPAREN
};

extern enum cpp_token macarg (struct cpp_reader *, int);

void
macroexpand (struct cpp_reader *pfile, struct definition *defn)
{
  int nargs = defn->nargs;

  if (nargs >= 0)
    {
      enum cpp_token token;  /* { dg-bogus "token" "uninitialized variable warning" { xfail *-*-* } } */
      int i, rest_args;
      i = 0;
      rest_args = 0;
      do
	{
	  if (rest_args)
	    continue;
	  if (i < nargs || (nargs == 0 && i == 0))
	    {
	      /* if we are working on last arg which absorbs rest of args... */
	      if (i == nargs - 1 && defn->rest_args)
		rest_args = 1;
	      token = macarg (pfile, rest_args);
	    }
	  else
	    token = macarg (pfile, 0);
	  if (token == CPP_EOF || token == CPP_POP)
	    return;

	  i++;
	}
      while (token == CPP_COMMA);
    }
}
