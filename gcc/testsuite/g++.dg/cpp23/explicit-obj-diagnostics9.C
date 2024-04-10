// PR c++/113340
// { dg-do compile { target c++23 } }

struct S {
  ~S(this S &) = default; // { dg-error "destructors may not have parameters" }
};
