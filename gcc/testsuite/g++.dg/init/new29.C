// PR c++/43951
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort ();

class Foo
{
public:
  Foo () : xxx (1) {};
  const int xxx;
};

struct Foo2
{
  Foo foo;
};

Foo2 *
bar ()
{
  return new Foo2;
}

int
main ()
{
  Foo2 *p = bar ();

  if (p->foo.xxx != 1)
    abort ();
  return 0;
}
