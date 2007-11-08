/* PR tree-optimization/32575 */

extern void abort (void);

struct S
{
  void *s1, *s2;
  unsigned char s3, s4, s5;
};

__attribute__ ((noinline))
void *
foo (void)
{
  static struct S s;
  return &s;
}

__attribute__ ((noinline))
void *
bar ()
{
  return (void *) 0;
}

__attribute__ ((noinline))
struct S *
test (void *a, void *b)
{
  struct S *p, q;
  p = foo ();
  if (p == 0)
    {
      p = &q;
      __builtin_memset (p, 0, sizeof (*p));
    }
  p->s1 = a;
  p->s2 = b;
  if (p == &q)
    p = 0;
  return p;
}

int
main (void)
{
  int a;
  int b;
  struct S *z = test ((void *) &a, (void *) &b);
  if (z == 0 || z->s1 != (void *) &a || z->s2 != (void *) &b || z->s3 || z->s4)
    abort ();
  return 0;
}
