// PR c++/49267
// { dg-options -std=c++11 }

struct X {
  operator int&();
  operator int&&();
};

int&&x = X();
