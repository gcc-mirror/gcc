/* { dg-do compile } */
/* { dg-options "-O2" } */

int
main (long a, long *b, long c)
{
  if (!c)
    return 0;
  int g;
  *b = (g & ~3000000000) < 0 ? a : a - g;
  while (1)
    ;
  return 0;
}
