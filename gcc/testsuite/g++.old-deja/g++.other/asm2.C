// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct C
{
  void f ();
};

void C::f ()
{
  asm ("" : : "m" (f)); // { dg-error "" } type could not be determined
}
