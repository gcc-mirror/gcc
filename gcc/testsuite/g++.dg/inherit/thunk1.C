// { dg-do run { target arm*-*-* aarch64*-*-* i?86-*-* x86_64-*-* s390*-*-* alpha*-*-* ia64-*-* sparc*-*-* } }
// { dg-skip-if "" { arm_thumb1_ok } }

#include <stdarg.h>

extern "C" void abort ();

struct A {
  virtual void f (int, ...) {}
  int i;
};

struct B : virtual public A {
};

struct C : public B {
  C ();
  virtual void f (int, ...);
};

extern C* cp;

C::C () { cp = this; }

void C::f (int i, ...) {
  if (this != cp)
    abort ();
  va_list ap;
  if (i != 3)
    abort ();
  va_start (ap, i);
  if (va_arg (ap, int) != 7)
    abort ();
  va_end (ap);
}

C* cp = new C;

int main () 
{
  cp->f (3, 7);
}
