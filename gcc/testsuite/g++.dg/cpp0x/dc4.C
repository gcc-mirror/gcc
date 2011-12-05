// { dg-do compile }
// { dg-options "--std=c++98" }

struct X {
  X() {}
  X(int) : X() {} // { dg-warning "delegating constructors" }
};
