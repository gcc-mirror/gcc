struct A { unsigned long buf[100]; };
int foo(unsigned int *x)
{
  unsigned int a;

  if (!x)
    return -22;

#ifdef __ia64__
  if (({
    register long b asm ("r8") = 0;
    register long c asm ("r9") = 0;
    asm ("" : "=r"(c), "=r"(b) : "m"(*(struct A *)x), "1"(b));
    a = (unsigned int) c;
    b; }))
    return -14;
#endif

  return 0;
}
