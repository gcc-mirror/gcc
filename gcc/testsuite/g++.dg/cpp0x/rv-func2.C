// PR c++/49458
// { dg-do compile { target c++11 } }

typedef void ftype();

struct A {
  operator ftype&(void);
};

ftype &&frvref = A();
