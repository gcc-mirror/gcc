// { dg-additional-options "-fmodule-header" }
// { dg-module-bmi {} }

struct X 
{
  int m;
  X (int m) :m(m) {}
  operator int () const { return m; }
};
