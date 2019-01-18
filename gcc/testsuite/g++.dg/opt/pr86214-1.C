// PR tree-optimization/86214
// { dg-do compile }
// { dg-options "-O2 -Wstack-usage=15000" }

typedef __SIZE_TYPE__ size_t;
struct A { A (); ~A (); int a; void qux (const char *); };
int bar (char *);

static inline A
foo ()
{
  char b[8192];
  int x = bar (b);
  A s;
  if (x > 0 && (size_t) x < sizeof b)
    s.qux (b);
  return s;
}

void
baz ()	// { dg-bogus "stack usage is" }
{
  A a;
  char c[1024];
  bar (c);
  foo (); foo (); foo (); foo (); foo ();
  foo (); foo (); foo (); foo (); foo ();
  foo (); foo (); foo (); foo (); foo ();
  foo (); foo (); foo (); foo (); foo ();
}
