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
    S s1;
  
  t:
    S s2;
    ;
  }

  goto t; // ERROR - jump avoids initialization of `s1'
}
