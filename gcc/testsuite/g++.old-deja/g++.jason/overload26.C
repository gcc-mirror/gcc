// { dg-do assemble  }
// Bug: g++ tries to build up a mangled name for its ideal match, which
// fails for one call below.

extern const char foo[];
extern const char baz[10];
extern const char *fred;

struct A {
  void f(const char *);
} *a;

void bing(const char *);
int main ()
{
	a->f(foo);		// { dg-bogus "" }  because foo's size unknown.
	a->f(baz);
	a->f(fred);
	bing(fred);
	bing(foo);
	bing(baz);
}
