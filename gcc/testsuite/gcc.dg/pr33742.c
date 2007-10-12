/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

typedef unsigned short ush;
extern ush prev[];

void fill_window ()
{
  register unsigned n, m;

  for (n = 0; n < 32768; n++)
    {
      m = prev[n];
      prev[n] = (ush) (m >= 0x8000 ? m - 0x8000 : 0);
    }
}
