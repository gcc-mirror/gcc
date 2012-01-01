// { dg-do assemble  }
// The standard sez that a use of a name gets the most access it can through
// the various paths that can reach it.  Here, the access decl in B gives
// us access.

struct A
{
  void f ();			
};

struct B: private virtual A
{
  A::f; // { dg-warning "deprecated" }
};

struct C: private virtual A, public B
{
};

int
main ()
{
  C c;

  c.f ();			
}
