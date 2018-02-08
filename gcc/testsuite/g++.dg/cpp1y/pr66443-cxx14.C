// { dg-do run { target c++14 } }

// pr c++/66443 a synthesized ctor of an abstract class that's deleted
// only because of virtual base construction doesn't stop a derived
// class using it as a base object constructor (provided it has a
// suitable ctor invocation of the virtual base).

static int a_made;

struct A {
  A *m_a = this;
  A (int) { a_made++; }
};

struct B : virtual A {
  A *m_b = this;
  virtual bool Ok () = 0; // abstract
};

struct C : B {
  // C::m_c is placed where a complete B object would put A
  int m_c = 1729;
public:
  C();
  virtual bool Ok ();
};

bool C::Ok ()
{
  // check everyone agreed on where A is
  return a_made == 1 && m_a == this && m_b == this && m_c == 1729;
}

C::C ()
  : A (1) // Explicit call of A's ctor
{  }

bool Ok (C &c)
{
  return true;
}

int main ()
{
  C c;

  return !c.Ok ();
}
