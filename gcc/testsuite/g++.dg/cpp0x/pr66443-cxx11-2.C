// { dg-do compile { target c++11_only } }

class C;

  
struct A {
  A ();
private:
  ~A (){ }
  friend class C;
};

struct B : virtual A {  // { dg-error "is private" }
  B ();
  virtual bool Ok () = 0; // abstract
};

struct C : B {  // { dg-error "use of deleted" }
  C ();
  virtual bool Ok ();
};

C c; // { dg-error "use of deleted" }
