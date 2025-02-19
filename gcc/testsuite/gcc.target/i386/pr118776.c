/* PR target/118776 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */

void bar (unsigned char *);

void
foo (unsigned char *x)
{
  unsigned char b[32];
  bar (b);
  for (int i = 0; i < 32; i++)
    {
      unsigned char c = 8;
      if (i > 3)
	{
	  unsigned char d = b[i];
	  d = 1 > d ? 1 : d;
	  c = d;
	}
      x[i] = c;
    }
}
