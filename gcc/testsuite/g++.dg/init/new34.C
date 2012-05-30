// PR c++/53356

struct A { A(); ~A(); };

struct B {
    operator const A () const;
};

A* cause_ICE() {
  return new A((A(),A()));
}
