/* { dg-options "-ffast-math" } */

float farg;
unsigned val;

void __attribute__((optimize("O")))
test()
{
  val = __builtin_ceilf(farg);
}
