/* { dg-additional-options "-std=gnu89" } */

foo (a, p)
     char a;
     int *p;
{
  int b = a;
  *p = b;
  a = (char) a;
  if (a)
    return b;
  else
    return b + 1;
}
