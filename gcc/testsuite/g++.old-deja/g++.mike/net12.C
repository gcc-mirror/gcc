// Build don't link:
// Special g++ Options: -pedantic-errors

struct Foo {
  char *p;
  const char *q;
  void m() const;
};

void other(char &x);	// ERROR - reference below

void
Foo::m() const
{
    other(*q);		// ERROR - this is bad
}
