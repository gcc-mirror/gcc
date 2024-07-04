/* { dg-additional-options "-std=gnu89" } */

foo (p)
     unsigned char *p;
{
  unsigned char a = 0;

  if (*p > 0)
    return 1;
  return 0;
}
