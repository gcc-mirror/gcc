// { dg-do assemble  }

class c {
  void (c::*x)();
public:
  void f() { this->x(); } // { dg-error "" } pointer-to-member
};
