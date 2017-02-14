// { dg-do compile { target c++14 } }

// pr c++/66443 a synthesized ctor of an abstract class that's deleted
// only because of virtual base construction doesn't stop a derived
// class using it as a base object constructor (provided it has a
// suitable ctor invocation of the virtual base).

// However we should still complain if the intermediate base is a
// non-abstract type.

static int a_made;

struct A {
  A *m_a = this;
  A (int) { a_made++; }
};

struct B : virtual A { // { dg-error "no matching function" }
  A *m_b = this;
  virtual bool Ok (); // not abstract
};

bool B::Ok ()
{
  return false;
}


B b; // { dg-error "deleted" }
