/* PR debug/45849 */
/* { dg-do compile } */
/* { dg-options "-g -Wno-uninitialized" } */

extern void bar (void);

void
foo (long repllen, char *rp)
{
  char *matchend;
  char *scan;
  long len;
  char *matchstart;
  char *text;
  char *t;

  repllen--;

  for (;;)
    {
      matchstart = t + rp[0];
      matchend = rp;
      len = matchstart - text + repllen * (matchend - matchstart);
      while (len)
	;
      for (scan = text; scan != rp; scan++)
	bar ();
      if (matchstart)
	text = matchend;
    }
}
