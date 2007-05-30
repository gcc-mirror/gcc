/* PR target/27387
   We used to generate a non-PIC thunk on thumb even with -fPIC.
   Make sure that won't happen anymore.  */

/* { dg-do compile } */
/* { dg-require-effective-target arm32 } */
/* { dg-options "-mthumb -fPIC" } */

struct A {
  virtual void f ();
};

struct B {
  virtual void g ();
};

struct C : public A, public B {
  virtual void g();
};

void
C::g()
{
}

/* { dg-final { scan-assembler "LTHUNKPC" } } */
