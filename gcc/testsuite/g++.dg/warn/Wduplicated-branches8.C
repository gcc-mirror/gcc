// PR c++/117880
// { dg-do compile }
// { dg-options "-Wduplicated-branches" }

struct X {};

template<typename>
struct S {
  void g1 () { false ? void() : void(); }   // { dg-warning "this condition has identical branches" }
  void g2 () { false ? int() : int(); }	    // { dg-warning "this condition has identical branches" }
  void g3 () { int() ? : int(); }	    // { dg-warning "this condition has identical branches" }
  void g4 () { false ? int() : double(); }
  void g5 () { false ? unsigned() : int(); }
  void g6 () { false ? signed() : int(); }  // { dg-warning "this condition has identical branches" }
  // Two different TARGET_EXPRs.
  void g7 () { false ? X() : X(); }
};
