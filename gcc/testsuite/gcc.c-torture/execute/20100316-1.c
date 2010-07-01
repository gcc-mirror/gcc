struct Foo {
  int i;
  unsigned precision : 10;
  unsigned blah : 3;
} f;

int __attribute__((noinline,noclone))
foo (struct Foo *p)
{
  struct Foo *q = p;
  return (*q).precision;
}

extern void abort (void);

int main()
{
  f.i = -1;
  f.precision = 0;
  f.blah = -1;
  if (foo (&f) != 0)
    abort ();
  return 0;
}
