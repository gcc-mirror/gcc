// PR c++/4286: We were crashing when trying to set up the class bindings in
// g(), because xref wanted the mangled name, which breaks inside a template.

// Of course, the offending code is actually ill-formed anyway, so check
// for the error.  Also check that it's formatted properly.

struct A
{
  template<class T> void f();
};

template<class T> void A::f()
{
  struct B
  {
    void g() {}
    static int x;	// { dg-error "static.*`int A::f\\(\\)::B::x'" "" }
  };
}

int main ()
{
  A a;
  a.f<int> ();
}
