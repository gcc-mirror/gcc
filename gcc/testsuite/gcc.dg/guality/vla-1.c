/* PR debug/43150 */
/* { dg-do run } */
/* { dg-options "-g" } */

void __attribute__((noinline))
bar (short *p)
{
  __builtin_memset (p, '\0', 17 * sizeof (*p));
  asm volatile ("" : : "r" (p) : "memory");
}

int __attribute__((noinline))
f1 (int i)
{
  char a[i + 1];
  a[0] = 5;		/* { dg-final { gdb-test .+1 "i" "5" } } */
  return a[0];		/* { dg-final { gdb-test . "sizeof (a)" "6" } } */
}

int __attribute__((noinline))
f2 (int i)
{
  short a[i * 2 + 7];	/* { dg-final { gdb-test .+1 "i" "5" } } */
  bar (a);		/* { dg-final { gdb-test . "sizeof (a)" "17 * sizeof (short)" } } */
  return a[i + 4];
}

int
main ()
{
  volatile int j;
  int i = 5;
  asm volatile ("" : "=r" (i) : "0" (i));
  j = f1 (i);
  f2 (i);
  return 0;
}
