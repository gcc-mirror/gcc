// { dg-do assemble  }
// { dg-options "-Wunused" }
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
    int j; // { dg-warning "" } unused
    
  t:       // { dg-warning "" } unused
    S s2;
  }
}
