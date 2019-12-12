// PR c++/77548
// { dg-do compile }
// { dg-options "" }

struct S
{ 
  int f (void) { return 0; }
  int f (int)  { return f ? : 1; } // { dg-error "cannot resolve overloaded function" }
};
