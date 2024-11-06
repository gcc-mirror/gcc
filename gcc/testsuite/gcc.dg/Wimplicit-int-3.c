/* { dg-do compile } */
/* { dg-options "-std=gnu17 -pedantic-errors -Wno-implicit-int" } */

static l;

foo (a)
{
  auto p;
  typedef bar;
}
