// { dg-do compile }
// { dg-options "-O1 -fno-signed-zeros" }

void
foo (double x, double y)
{
  if (!x == !y * -1.0)
    __builtin_trap ();
}
