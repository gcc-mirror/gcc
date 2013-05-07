/* Test that stack protection is done on chosen functions. */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fstack-protector-strong" } */

class A
{
public:
  A() {}
  ~A() {}
  void method();
  int state;
};

/* Frame address exposed to A::method via "this". */
int
foo1 ()
{
  A a;
  a.method ();
  return a.state;
}

/* Possible destroying foo2's stack via &a. */
int
global_func (A& a);

/* Frame address exposed to global_func. */
int foo2 ()
{
  A a;
  return global_func (a);
}

/* { dg-final { scan-assembler-times "stack_chk_fail" 2 } } */
