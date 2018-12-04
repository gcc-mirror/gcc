// { dg-additional-options "-fmodule-legacy" }
// { dg-module-bmi {"leg-merge-6_b.H"} }

struct X 
{
  int m;
  X (int m) :m(m) {}
  operator int () const { return m; }
};
