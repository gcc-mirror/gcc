// PR c++/17685

struct S {
  operator int; // { dg-error "" }
  operator void; // { dg-error "" }
};

