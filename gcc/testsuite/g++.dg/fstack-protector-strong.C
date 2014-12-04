/* Test that stack protection is done on chosen functions. */

/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fstack-protector-strong" } */

/* This test checks the presence of __stack_chk_fail function in assembler.
 * Compiler generates _stack_chk_fail_local (wrapper) calls instead for PIC.
 */
/* { dg-require-effective-target nonpic } */

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

/* Frame addressed exposed through return slot. */

struct B
{
  /* Discourage passing this struct in registers. */
  int a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
  int method ();
  B return_slot();
};

B global_func ();
void noop ();

int foo3 ()
{
  return global_func ().a1;
}

int foo4 ()
{
  try {
    noop ();
    return 0;
  } catch (...) {
    return global_func ().a1;
  }
}

int foo5 ()
{
  try {
    return global_func ().a1;
  } catch (...) {
    return 0;
  }
}

int foo6 ()
{
  B b;
  return b.method ();
}

int foo7 (B *p)
{
  return p->return_slot ().a1;
}

/* { dg-final { scan-assembler-times "stack_chk_fail" 7 } } */
