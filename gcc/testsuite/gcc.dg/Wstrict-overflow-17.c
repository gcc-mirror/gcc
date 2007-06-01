/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow" } */

/* This erroneously gave an overflow warning.  */

extern void bar (char);
void
foo (char *s)
{
  int len, i;

  for (len = 1; len < 5000; ++len)
    {
      for (i = 0; i < len; ++i)
	{
	  if (s[i] != '\0')
	    bar (s[i]);
	}
    }
}
