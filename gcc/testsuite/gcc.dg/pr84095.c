/* PR tree-optimization/84095 - false-positive -Wrestrict warnings for
   memcpy within array
   { dg-do compile }
   { dg-options "-O2 -Wrestrict" } */

struct { int i; } a[8];

void f (void)
{
  int i;

  for (i = 1; i < 8; i++)
    __builtin_memcpy (&a[i], &a[0], sizeof(a[0]));   /* { dg-bogus "\\\[-Wrestrict]" } */
}
