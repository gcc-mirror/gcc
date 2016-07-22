// { dg-do run }
// { dg-skip-if "fails with generic thunk support" { rs6000-*-* powerpc-*-eabi v850-*-* sh-*-* h8*-*-* xtensa*-*-* m32r*-*-* lm32-*-* nios2-*-* } { "*" } { "" } }
// Test that variadic function calls using thunks work right.
// Note that this will break on any target that uses the generic thunk
//  support, because it doesn't support variadic functions.


#include <stdarg.h>

struct A {
  void* p;
  A (void* q): p (q) { }
  A (const A& a): p (a.p) { }
};

class CBase {
public:
   virtual void BaseFunc();
};

class MMixin {
public:
   virtual A MixinFunc(int arg, ...) = 0;
};

class CExample : public CBase, public MMixin {
public:
   A MixinFunc(int arg, ...);
};

void CBase::BaseFunc()
{
}

A CExample::MixinFunc(int arg, ...)
{
  va_list ap;
  va_start (ap, arg);

  if (arg != 1 || va_arg (ap, int) != 2 || va_arg (ap, int) != 3
      || va_arg (ap, int) != 4 || va_arg (ap, int) != 5
      || va_arg (ap, int) != 6 || va_arg (ap, int) != 7
      || va_arg (ap, int) != 8 || va_arg (ap, int) != 9)
    return 0;
  return this;
}

void* test(MMixin& anExample)
{
  return anExample.MixinFunc(1,2,3,4,5,6,7,8,9).p;
}

int main ()
{
  CExample c;

  if (test(c) != &c)
    return 1;
}
