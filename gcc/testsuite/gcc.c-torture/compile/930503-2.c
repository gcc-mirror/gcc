/* { dg-additional-options "-std=gnu89" } */

f()
{
  struct { char x; } r;
  g(r);
}
