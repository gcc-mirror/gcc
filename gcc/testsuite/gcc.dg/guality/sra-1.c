/* PR debug/43983 */
/* { dg-do run } */
/* { dg-options "-g" } */

struct A { int i; int j; };
struct B { int : 4; int i : 12; int j : 12; int : 4; };

__attribute__((noinline)) void
bar (int x)
{
  asm volatile ("" : : "r" (x) : "memory");
}

__attribute__((noinline)) int
f1 (int k)
{
  struct A a = { 4, k + 6 };
  asm ("" : "+r" (a.i));
  a.j++;
  bar (a.i);		/* { dg-final { gdb-test 21 "a.i" "4" } } */
  bar (a.j);		/* { dg-final { gdb-test 21 "a.j" "14" } } */
  return a.i + a.j;
}

__attribute__((noinline)) int
f2 (int k)
{
  int a[2] = { 4, k + 6 };
  asm ("" : "+r" (a[0]));
  a[1]++;
  bar (a[0]);		/* { dg-final { gdb-test 32 "a\[0\]" "4" } } */
  bar (a[1]);		/* { dg-final { gdb-test 32 "a\[1\]" "14" } } */
  return a[0] + a[1];
}

__attribute__((noinline)) int
f3 (int k)
{
  struct B a = { 4, k + 6 };
  asm ("" : "+r" (a.i));
  a.j++;
  bar (a.i);		/* { dg-final { gdb-test 43 "a.i" "4" } } */
  bar (a.j);		/* { dg-final { gdb-test 43 "a.j" "14" } } */
  return a.i + a.j;
}

int
main (void)
{
  int k;
  asm ("" : "=r" (k) : "0" (7));
  f1 (k);
  f2 (k);
  f3 (k);
  return 0;
}
