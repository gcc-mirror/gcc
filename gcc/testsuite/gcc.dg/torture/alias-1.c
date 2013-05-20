/* { dg-do run } */
/* { dg-options "-fschedule-insns" } */
/* { dg-require-effective-target scheduling } */

extern void abort (void) __attribute__((noreturn));

struct B { int a; int b;};
struct wrapper {
union setconflict
{
  struct S { char one1; struct B b1; } s;
  struct T { struct B b2; char two2; } t;
} a;
};

int
main ()
{
  int sum = 0;
  int i;
  struct wrapper w;
  struct B *p;

  p = &w.a.s.b1;
  asm ("": "=r" (p):"0" (p));
  p->a = 0;
  asm ("": "=r" (p):"0" (p));
  sum += p->a;

  p = &w.a.t.b2;
  asm ("": "=r" (p):"0" (p));
  p->b = 1;
  asm ("": "=r" (p):"0" (p));
  sum += p->b;

  if (sum != 1)
    abort();
  return 0;
}
