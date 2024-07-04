/* { dg-do run } */
/* { dg-options "-g -fno-ipa-icf" } */

int __attribute__((noipa))
get_val1 (void)  {return 20;}
int __attribute__((noipa))
get_val2 (void)  {return 7;}

void __attribute__((noipa))
use (int x)
{
  asm volatile ("" : : "r" (x) : "memory");
}

static int __attribute__((noinline))
bar (int i, int k)
{
  asm ("" : "+r" (i));
  use (i);		/* { dg-final { gdb-test . "k" "3" { xfail { ! { *-*-*-* && { any-opts "-O0" "-O1" "-Og" } } } } } } */
  return 6 + get_val1();
}

volatile int v;

static int __attribute__((noinline))
foo (int i, int k)
{
  int r;
  v = 9;
  k = (k + 14)/k;
  r = bar (i, k);		/* { dg-final { gdb-test . "k" "3" } } */
  return r;
}

volatile int v;

int
main (void)
{
  int k = get_val2 ();
  int r = foo (get_val1 (), k);
  v = r + k;   /* k has to live accross the call or all is probably lost  */
  return 0;
}
