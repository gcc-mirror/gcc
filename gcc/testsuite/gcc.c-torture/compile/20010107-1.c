/* { dg-require-effective-target indirect_calls } */

unsigned long x[4];

void foo(void)
{
  ((void (*)())(x+2))();
}
