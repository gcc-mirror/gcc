// { dg-do compile }
// Contributed by Wolfgang Bangerth <bangerth at dealii dot org>
// PR c++/12335: Make sure we don't ICE on the qualified-id form of a 
//  destructor call.

struct Y { 
  ~Y() {}      // { dg-bogus "note" "implemented DR272" { xfail *-*-* } }  
};

struct X : Y { 
  ~X() {}      // { dg-bogus "note" "implemented DR272" { xfail *-*-* } }  
  void f() { 
    X::~X();   // { dg-bogus "" "implemented DR272" { xfail *-*-* } }  
    Y::~Y();   // { dg-bogus "" "implemented DR272" { xfail *-*-* } }  
  } 
};
