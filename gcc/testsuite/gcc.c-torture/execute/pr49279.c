/* PR tree-optimization/49279 */
extern void abort (void);

struct S { int a; int *__restrict p; };

__attribute__((noinline, noclone))
struct S *bar (struct S *p)
{
  struct S *r;
  asm volatile ("" : "=r" (r) : "0" (p) : "memory");
  return r;
}

__attribute__((noinline, noclone))
int
foo (int *p, int *q)
{
  struct S s, *t;
  s.a = 1;
  s.p = p;
  t = bar (&s);
  t->p = q;
  s.p[0] = 0;
  t->p[0] = 1;
  return s.p[0];
}

int
main ()
{
  int a, b;
  if (foo (&a, &b) != 1)
    abort ();
  return 0;
}
