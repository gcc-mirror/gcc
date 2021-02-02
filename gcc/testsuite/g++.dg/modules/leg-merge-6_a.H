// { dg-module-do run }
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

struct X 
{
  int m;
  X (int m) :m(m) {}
  operator int () const { return m; }
};
