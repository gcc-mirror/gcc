// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  S ();
  ~S ();
};

void f ()
{
  {
    S s1; // ERROR - skips initialization
  
  t:	  // ERROR - jump to label
    S s2;
    ;
  }

  goto t; // ERROR - from here
}
