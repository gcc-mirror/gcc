// { dg-do assemble  }
// { dg-options "-pedantic-errors" }

struct Foo {
  char *p;
  const char *q;
  void m() const;
};

void other(char &x);

void
Foo::m() const
{
    other(*p);		// this is legal
}
