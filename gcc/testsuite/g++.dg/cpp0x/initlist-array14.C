// PR c++/104300
// { dg-do compile { target c++11 } }

struct ss {
  char r;
  ss();
};
struct a {
  ss e[6];
};
a vv;
void ff() { vv = {}; }
