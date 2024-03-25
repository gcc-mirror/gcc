/* { dg-additional-options "-std=gnu89" } */

foo (char *a)
{
  char b;
  int c;
  b = *a;
  c = b;
  if (c < 0)
    return 1;
  a[1] = b;

}
