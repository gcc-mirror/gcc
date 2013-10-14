/* PR middle-end/58670 */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */

#if defined (__i386__) || defined (__x86_64__)
#define ASM_STR "btsl $1, %0; jc %l[lab]"
#endif

__attribute__((noinline, noclone)) int
foo (int a, int b)
{
  if (a)
    return -3;
#ifdef ASM_STR
  asm volatile goto (ASM_STR : : "m" (b) : "memory" : lab);
  return 0;
lab:
#endif
  return 0;
}

int
bar (int a, int b)
{
  if (a)
    return -3;
#ifdef ASM_STR
  asm volatile goto (ASM_STR : : "m" (b) : "memory" : lab);
  return 0;
lab:
#endif
  return 0;
}

int
main ()
{
  if (foo (1, 0) != -3
      || foo (0, 3) != 0
      || foo (1, 0) != -3
      || foo (0, 0) != 0
      || bar (1, 0) != -3
      || bar (0, 3) != 0
      || bar (1, 0) != -3
      || bar (0, 0) != 0)
    __builtin_abort ();
  return 0;
}
