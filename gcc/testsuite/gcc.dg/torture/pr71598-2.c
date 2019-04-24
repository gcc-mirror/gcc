/* { dg-do run } */
/* { dg-prune-output "use of enum values across objects may fail" } */
/* { dg-additional-options "-fshort-enums" } */

enum e1 { c1 = -__INT_MAX__ };

__attribute__((noinline,noclone))
int f(enum e1 *p, signed int *q)
{
  *p = c1;
  *q = 2;
  return *p;
}

enum e2 { c2 = __SHRT_MAX__ + 1};

__attribute__((noinline,noclone))
int g(enum e2 *p, unsigned short *q)
{
  *p = c2;
  *q = 2;
  return *p;
}

enum e3 { c3 = __SCHAR_MAX__ };

__attribute__((noinline,noclone))
int h(enum e3 *p, unsigned char *q)
{
  *p = c3;
  *q = 2;
  return *p;
}

int main()
{
  signed x;
  unsigned short y;
  unsigned char z;

  if (f(&x, &x) != 2)
    __builtin_abort();
  if (g(&y, &y) != 2)
    __builtin_abort();
  if (h(&z, &z) != 2)
    __builtin_abort();
  return 0;
}
