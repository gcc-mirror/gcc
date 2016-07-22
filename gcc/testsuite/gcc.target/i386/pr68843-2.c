/* { dg-do run } */
/* { dg-options "" } */

int
__attribute__((noinline, noclone))
test (double y)
{
  int a, b;
  asm ("fistp{l (%1)| DWORD PTR [%1]}\n\t"
       "mov{l (%1), %0| %0, DWORD PTR [%1]}"
       : "=r" (a)
       : "r" (&b), "t" (y)
       : "st");
  return a;
}

int
main ()
{
  int t = -10;

  if (test (t) != t)
    __builtin_abort ();
  return 0;
}
