// PR c++/49267
// { dg-options -std=c++0x }

struct X {
  operator int&();
  operator int&&();
};

int&&x = X();
