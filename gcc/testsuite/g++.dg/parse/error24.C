// PR c++/19395

struct A {
  typedef int ::X; // { dg-error "" }
};


