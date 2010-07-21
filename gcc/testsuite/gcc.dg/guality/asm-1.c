/* { dg-do run } */
/* { dg-options "-g" } */

struct A { int x; unsigned short y; char z[64]; };

void __attribute__((noinline))
foo (struct A *p, char *q)
{
  int f = &p->z[p->y] - q;
  asm volatile ("nop");
  asm volatile ("nop" : : "g" (f));		/* { dg-final { gdb-test 12 "f" "14" } } */
  asm volatile ("" : : "g" (p), "g" (q));
}

int
main ()
{
  struct A a;
  __builtin_memset (&a, 0, sizeof a);
  a.y = 26;
  a.x = 12;
  asm volatile ("" : : "r" (&a) : "memory");
  foo (&a, &a.z[a.x]);
  return 0;
}
