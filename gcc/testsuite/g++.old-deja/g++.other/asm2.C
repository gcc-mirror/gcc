// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct C
{
  void f ();
};

void C::f ()
{
  asm ("" : : "m" (f)); // ERROR - type could not be determined
}
