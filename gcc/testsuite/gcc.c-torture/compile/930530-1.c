/* { dg-additional-options "-std=gnu89" } */

f ()
{
  struct { char a, b; } x;
  g (x, x, x, x);
}
