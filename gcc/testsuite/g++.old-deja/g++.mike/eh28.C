// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe

#include <typeinfo>

int fail = 1;

class X            { public: virtual void p() { } };
class Y : public X { public: virtual void p() { fail = 0; } };

main()
{
  try          { Y y; throw y; }
  catch (X& x) { x.p();  }
  catch (...)  { }
  return fail;
}
