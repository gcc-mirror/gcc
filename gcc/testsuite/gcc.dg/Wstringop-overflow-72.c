/* PR middle-end/101854 - Invalid warning -Wstringop-overflow wrong argument
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

struct A { int a[5]; };

struct A g (int*, int[6][8]);

struct A f (void)
{
  int a[2];
  return g (a, 0);            // { dg-bogus "-Wstringop-overflow" }
}
