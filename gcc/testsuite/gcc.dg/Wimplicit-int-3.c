/* { dg-do compile } */
/* { dg-options "-pedantic-errors -Wno-implicit-int" } */

static l;

foo (a)
{
  auto p;
  typedef bar;
}
