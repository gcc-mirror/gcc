int
__attribute__((noinline, noclone))
test (double y)
{
  int a, b;
  asm ("fistpl (%1)\n\t"
       "movl (%1), %0"
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
