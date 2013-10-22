// PR c++/49458
// { dg-options -std=c++11 }

typedef void ftype();

struct A {
  operator ftype&(void);
};

ftype &&frvref = A();
