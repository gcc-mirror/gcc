// PR c++/84593
// { dg-do compile { target c++14 } }

struct a {
  int x;
  int c = 0;
  int &b;
} c = {}; // { dg-error "uninitialized reference" }
