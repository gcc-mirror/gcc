// { dg-do assemble  }
namespace X{
  typedef int foo;
  const int bar=2;
  namespace Y{
    void f(foo);
    extern int g;
    extern int g1;
    struct h{
      void i(foo);
    };
  }
}

void X::Y::f(foo)
{
}

int X::Y::g = bar;
int X::Y::g1(bar);

void X::Y::h::i(foo)
{}
