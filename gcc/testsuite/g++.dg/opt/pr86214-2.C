// PR tree-optimization/86214
// { dg-do compile }
// { dg-options "-O2 -Wstack-usage=15000" }

typedef __SIZE_TYPE__ size_t;
struct A { A (); ~A (); int a; void qux (const char *); };
int bar (char *);

static inline __attribute__((always_inline)) A
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
  foo (); foo (); foo (); foo (); foo ();
  foo (); foo (); foo (); foo (); foo ();
  foo (); foo (); foo (); foo (); foo ();
  foo (); foo (); foo (); foo (); foo ();
}
