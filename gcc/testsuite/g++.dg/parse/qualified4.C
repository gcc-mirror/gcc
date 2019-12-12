// PR c++/16782
// { dg-options "" }

struct X { 
  void X::bar() {} // { dg-error "8:extra qualification" }
}; 
