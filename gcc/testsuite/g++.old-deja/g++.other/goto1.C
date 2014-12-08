// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

struct S
{
  S ();
  ~S ();
};

void f ()
{
  {
    S s1; // { dg-message "" } skips initialization
  
  t:	  // { dg-error "" } jump to label
    S s2;
    ;
  }

  goto t; // { dg-message "" } from here
}
