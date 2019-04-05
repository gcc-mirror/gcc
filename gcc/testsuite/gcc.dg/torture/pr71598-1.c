/* { dg-do run } */
/* { dg-prune-output "use of enum values across objects may fail" } */
/* { dg-additional-options "-fno-short-enums" } */

enum e1 { c1 };

__attribute__((noinline,noclone))
int f(enum e1 *p, unsigned *q)
{
  *p = c1;
  *q = 2;
  return *p;
}

int main()
{
  unsigned x;

  if (f(&x, &x) != 2)
    __builtin_abort();
  return 0;
}
