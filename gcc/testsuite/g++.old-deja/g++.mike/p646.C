// { dg-do assemble  }
// { dg-options "-Wno-deprecated -Wreturn-type" }
// GROUPS passed i960
/*
  Bug Id: bnr
  PMRS Id: p0000646
  Bug is: Urgent Code Generation Problem in gcc-i960 V 1.95
*/



extern "C"
{
  int printf (const char *, ...);
  void abort ();
}

struct foo
{
  static int si;
  int i;
  foo ();
  foo (const foo&);
  ~foo ();
};

int
foo_parm_returns_i (foo foo_arg)
{
  return foo_arg.i;
}

int foo::si = 0;

foo::foo ()
{
  si++;
  printf ("new foo @ 0x%x; now %d foos\n", this, si);
}

foo::foo (const foo &other)
{
  si++;
  printf ("another foo @ 0x%x; now %d foos\n", this, si);
  *this = other;
}

foo::~foo ()
{
  si--;
  printf ("deleted foo @ 0x%x; now %d foos\n", this, si);
}

int
return_1 ()
{
  foo f;
  printf ("returning 1\n");
  return 1;
}

int
return_arg (int arg)
{
  foo f;
  printf ("returning %d\n", arg);
  return arg;
}

int
return_sum (int x, int y)
{
  foo f;
  printf ("returning %d+%d\n", x, y);
  return x + y;
}

foo
return_foo ()
{
  foo f;
  printf ("returning foo\n");
  return f;
}

foo
foo_parm_returns_foo (foo f)
{
  return f;
}

void
abort_because (const char *str)
{
  printf ("aborting because %s\n", str);
  abort ();
}

int
warn_return_1 ()
{
  foo f;
  printf ("returning 1\n");
}                              // { dg-warning "" } control reaches end

int
warn_return_arg (int arg)
{
  foo f;
  printf ("returning %d\n", arg);
  arg;
}                              // { dg-warning "" } control reaches end

int
warn_return_sum (int x, int y)
{
  foo f;
  printf ("returning %d+%d\n", x, y);
  x + y;
}                              // { dg-warning "" } control reaches end

foo
warn_return_foo ()
{
  foo f;
  printf ("returning foo\n");
}                              // { dg-warning "" } control reaches end

foo
warn_foo_parm_returns_foo (foo f)
{
  f;
}                              // { dg-warning "" } control reaches end

main ()			       // { dg-warning "" } no type
{
  int ii = return_1 ();
  if (ii != 1)
    abort_because ("wrong value returned");
  int j = return_arg (42);
  if (j != 42)
    abort_because ("wrong value returned");
  int k = return_sum (-69, 69);
  if (k != 0)
    abort_because ("wrong value returned");
  foo f1 = return_foo ();
  if (foo::si != 1)
    abort_because ("wrong number of foos");
  f1.i = 5;
  int l = foo_parm_returns_i (f1);
  if (l != 5)
    abort_because ("l != 5");
  foo f2 = foo_parm_returns_foo (f1);
  if (foo::si != 2)
    abort_because ("wrong number of foos");
  if (f2.i != 5)
    abort_because ("f2.i != 5");
  foo f3 = return_foo ();
  if (foo::si != 3)
    abort_because ("wrong number of foos");
  printf("PASS\n");
  return 0;
}
