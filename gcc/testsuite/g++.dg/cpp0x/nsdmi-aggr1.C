// PR c++/65815
// { dg-do compile { target c++11 } }

struct array {
  int data [2];
};

struct X {
  array a = { 1, 2 };
};
