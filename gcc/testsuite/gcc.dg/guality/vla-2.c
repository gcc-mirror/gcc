/* PR debug/42801 */
/* { dg-do run } */
/* { dg-options "-g" } */

void __attribute__((noinline))
fn1 (int *x, int y)
{
  asm volatile ("" : : "rm" (x), "rm" (y) : "memory");
}

static inline __attribute__((always_inline)) int
fn2 (int i)
{
  int a[i];
  fn1 (a, i);
  fn1 (a, i);		/* { dg-final { gdb-test . "sizeof (a)" "5 * sizeof (int)" } } */
  return i;
}

static inline __attribute__((always_inline)) int
fn3 (int i)
{
  int a[i];
  fn1 (a, i);
  fn1 (a, i);		/* { dg-final { gdb-test . "sizeof (a)" "6 * sizeof (int)" } } */
  return i;
}

static inline __attribute__((always_inline)) int
fn4 (int i)
{
  return fn3 (i);
}

int __attribute__((noinline))
fn5 (void)
{
  return fn2 (5) + 1;
}

int __attribute__((noinline))
fn6 (int i)
{
  return fn2 (i + 1) + fn4 (i + 2) + fn4 (i + 2) + 1;
}

int
main (void)
{
  int x = 4;
  asm volatile ("" : "+r" (x));
  fn5 ();
  fn6 (x);
  return 0;
}
