// PR debug/46815
// { dg-do run }
// { dg-options "-g" }
// { dg-skip-if "" { *-*-* }  { "*" } { "-O0" } }

struct S
{
  int i;
  S () { i = 42; }
  virtual void foo (void) {}
};

S
bar ()
{
  S s;
  return s;	// { dg-final { gdb-test 17 "s.i" "42" } }
}

int
main ()
{
  S s = bar ();
  return s.i - 42;
}
