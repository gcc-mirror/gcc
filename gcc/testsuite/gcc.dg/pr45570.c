/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O3 -fselective-scheduling2 -fsel-sched-pipelining -fsel-sched-pipelining-outer-loops -ftracer" } */
void
parser_get_next_char (char c, int qm, char *p)
{
  int quote_mode = 0;
  for (; *p; p++)
    {
      if (qm)
	{
	  if (quote_mode == 0 && *p == '"' && *(p - 1))
	    {
	      quote_mode = 1;
	      continue;
	    }
	  if (quote_mode && *p == '"' && *(p - 1))
	    quote_mode = 0;
	}
      if (quote_mode == 0 && *p == c && *(p - 1))
	break;
    }
}

void
parser_get_next_parameter (char *p)
{
  parser_get_next_char (':', 1, p);
}
