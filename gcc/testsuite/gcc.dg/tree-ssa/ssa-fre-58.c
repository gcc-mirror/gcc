/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

long long int a = -465274079317386463LL;
int b = 856872806;
int c = -1940894202;
int d = 1718449211;
int e = -392681565;
unsigned long long int f = 13521452247506316486ULL;
int g = -13194608;

__attribute__((noinline, noclone))
void foo ()
{
  if (!a - a)
    c = b = 0;
  else
    d = 3UL * a == 0;
  if (g / a)
    e = 0 < -a + 500849970701012771LL + (unsigned long) -a;
  else
    f = 4081116982543369LL & a;
}

int
main ()
{
  asm volatile ("" : : : "memory");
  foo ();
  if (f != 2818598057803777LL)
    __builtin_abort ();
  return 0;
}

/* Should CSE all loads of a.  */
/* { dg-final { scan-tree-dump-times " = a;" 1 "fre1" } } */
