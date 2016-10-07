// { dg-do compile { target c++14 } }

// DR 1658, inaccessible dtor of virtual base doesn't affect an
// abstract class

int a_unmade;

class C;

  
struct A {
private:
  ~A (){ a_unmade++; }
  friend class C;
};

struct B : virtual A {
  virtual bool Ok () = 0; // abstract
};

struct C : B {
  virtual bool Ok ();
};

C c;
