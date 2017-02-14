// { dg-do compile { target c++11_only } }

// pr c++/66443 it is still ill-formed in C++ 11 for a synthesized
// ctor that's deleted only because of virtual base construction

static bool a_made;

struct A { // { dg-message "candidate" }
  A( int ) { a_made = true; } // { dg-message "candidate" }
};

struct B: virtual A { // { dg-message "no matching function" }
  int m;
  virtual void Frob () = 0;
};

class C: public B {
public:
  C();
  virtual void Frob ();
};

void C::Frob ()
{
}

C::C ()
  : A( 1 ) // { dg-error "deleted function" }
{ }

