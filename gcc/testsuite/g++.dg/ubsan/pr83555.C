// PR c++/83555
// { dg-do run }
// { dg-options "-fsanitize=null" }
// { dg-output ":25:\[^\n\r]*reference binding to null pointer of type 'struct C'" }

struct A { int a; };
struct B { int b; };
struct C : A, B { int c; };

__attribute__((noipa)) C *
foo (B *b)
{
  return static_cast<C *>(b);
}

__attribute__((noipa)) C *
bar (B *b)
{
  return &static_cast<C &>(*b);
}

__attribute__((noipa)) C *
baz (B *b)
{
  return &static_cast<C &>(*b);
}

int
main ()
{
  C c;
  if (foo (static_cast<B *> (&c)) != &c)
    __builtin_abort ();
  if (foo (0))
    __builtin_abort ();
  if (bar (static_cast<B *> (&c)) != &c)
    __builtin_abort ();
  baz (0);
  return 0;
}
