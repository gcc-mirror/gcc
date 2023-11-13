/* { dg-additional-options "-std=gnu89" } */

omit (a, b)
    char a;
    char *b;
{
  char x;
  int i;
  x = *b;
  b[1] = x;
  foo ((int)x);
  return x + 1;
}
