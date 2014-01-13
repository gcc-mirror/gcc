extern void abort (void);

static struct X { void *a; void *b; } a, b;

void __attribute__((noinline)) foo (void)
{
  void *tem = a.b;
  a.b = (void *)0;
  b.b = tem;
  b.a = a.a;
  a.a = tem;
}

int main()
{
  a.b = &a;
  foo ();
  if (b.b != &a)
    abort ();
  return 0;
}
