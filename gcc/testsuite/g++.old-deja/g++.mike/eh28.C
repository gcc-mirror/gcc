// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

#include <typeinfo>

int fail = 1;

class X            { public: virtual void p() { } };
class Y : public X { public: virtual void p() { fail = 0; } };

int
main()
{
  try          { Y y; throw y; }
  catch (X& x) { x.p();  }
  catch (...)  { }
  return fail;
}
