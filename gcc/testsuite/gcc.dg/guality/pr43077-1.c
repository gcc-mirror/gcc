/* PR debug/43077 */
/* { dg-do run } */
/* { dg-options "-g -fno-ipa-icf" } */

int varb;

int __attribute__((noinline))
fn1 (void)
{
  int vara = (varb == 3);		/* { dg-final { gdb-test 11 "vara" "0" } } */
  asm volatile ("" : : "g" (vara));	/* { dg-final { gdb-test 11 "varb" "2" } } */
  return 0;
}

int __attribute__((noinline))
fn2 (void)
{
  int vara = (varb == 3);		/* { dg-final { gdb-test 19 "vara" "1" } } */
  asm volatile ("" : : "g" (vara));	/* { dg-final { gdb-test 19 "varb" "3" } } */
  return 0;
}

int __attribute__((noinline))
foo (unsigned long *p, unsigned long *q)
{
  int ret;
  asm volatile ("" : "=r" (ret), "=r" (*p), "=r" (*q) : "0" (1), "1" (2), "2" (3));
  return ret;
}

int __attribute__((noinline))
fn3 (void)
{
  unsigned long a = 0, b = 0, c = 0;
  a = foo (&b, &c);
					/* { dg-final { gdb-test 42 "a" "1" } } */
					/* { dg-final { gdb-test 42 "b" "2" } } */
					/* { dg-final { gdb-test 42 "c" "3" } } */
  unsigned long vara = a;		/* { dg-final { gdb-test 42 "vara" "1" } } */
  unsigned long varb = b;		/* { dg-final { gdb-test 42 "varb" "2" } } */
  unsigned long varc = c;		/* { dg-final { gdb-test 42 "varc" "3" } } */
  asm volatile ("" : : "g" (vara), "g" (varb), "g" (varc));
  return a;
}

int
main (void)
{
  asm volatile ("" : "=r" (varb) : "0" (2));
  fn1 ();
  asm volatile ("" : "=r" (varb) : "0" (3));
  fn2 ();
  fn3 ();
  return 0;
}
