// PR c++/14401

union U {
  int& i; // { dg-error "" }
};
