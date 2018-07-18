// { dg-do run }
// { dg-options "-O" }

struct Foo
{
  Foo() : a(1), b(1), c('a') {}
  int a;
  int b;
  char c;
};

static Foo copy_foo(Foo) __attribute__((noinline, noclone));

static Foo copy_foo(Foo A)
{
  return A;
}

struct Bar : Foo
{
  Bar(Foo t) : Foo(copy_foo(t)) {}
};

Foo F;

int main (void)
{
  Bar B (F);

  if (B.a != 1 || B.b != 1 || B.c != 'a')
    __builtin_abort ();

  return 0;
}
