/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void f (int, int);
void g (int a, int b)
{
  int i, j;
  for (i = a; i <= b; ++i)
    __builtin_memcpy (g, f, 6);
  for (j = a; j <= b; ++j)
    f(j, i);
}

