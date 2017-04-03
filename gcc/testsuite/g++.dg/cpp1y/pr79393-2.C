// { dg-do compile { target c++14 } }

// DR 1658, inaccessible dtor of virtual base doesn't affect an
// abstract class.  But we should stil check access to non-virtual bases.

class C;

struct A {
private:
  ~A (){  }
  friend class C;
};

struct B : A { // { dg-error "is private" }
  virtual bool Ok () = 0; // abstract
};

struct C : B {
  ~C () 
  { }  // { dg-error "use of deleted" }
  virtual bool Ok ();
};
