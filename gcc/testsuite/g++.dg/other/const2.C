// PR c++/3331: just because 'this' is readonly and bars[0].b is readonly
// doesn't mean that the result of the member reference is readonly.

struct foo
{
  int a;

  struct bar
  { int foo::* b ;};

  static const bar bars[];

  void bad ()
  {
    this->*(bars[0].b) = 42; // { dg-bogus "read-only" }
  }
};

const foo::bar foo::bars[] = { { &foo::a } };

int main ()
{ }
