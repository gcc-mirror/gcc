// PR tree-optimization/42890
// { dg-do compile }

extern "C" int puts (const char *) throw ();

struct S
{
  const char *a;
  const char **b;
  S (const char *s) { a = s; b = &a; }
  ~S () { puts (a); }
};

void
foo (int (*fn) (const char *))
{
  S a ("foo");
  fn ("bar");
}

int
main ()
{
  foo (puts);
}
