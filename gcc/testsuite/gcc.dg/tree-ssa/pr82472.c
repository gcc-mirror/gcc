/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution" } */

long int xj;

int
cx (long int *ox, short int mk, char tf)
{
  int si, f9;
  char *p4 = &tf;
  short int *rm = (tf != 0) ? (short int *)&f9 : &mk;

  for (f9 = 0; f9 < 2; ++f9)
    {
      *rm = 0;
      *p4 = *ox;
      si = mk;
      xj = 0;
      while (p4 < (char *)rm)
        ++p4;
    }

  return si;
}
