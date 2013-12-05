/* { dg-do run } */
/* { dg-additional-options "-ftree-slp-vectorize" } */

extern void abort (void);

static struct X { void *a; void *b; } a, b;
static struct X *p;

void __attribute__((noinline))
foo (void)
{
  void *tem = a.b;
  p->b = (void *)0;
  b.b = tem;
  b.a = a.a;
}

int main()
{
  p = &a;
  a.b = &a;
  foo ();
  if (b.b != &a)
    abort ();
  return 0;
}
