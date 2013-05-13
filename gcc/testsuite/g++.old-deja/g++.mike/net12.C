// { dg-do assemble  }
// { dg-options "-pedantic-errors" }

struct Foo {
  char *p;
  const char *q;
  void m() const;
};

void other(char &x);	// { dg-message "" } reference below

void
Foo::m() const
{
    other(*q);		// { dg-error "" } this is bad
}
