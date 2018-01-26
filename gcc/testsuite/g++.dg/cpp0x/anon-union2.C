// PR c++/83956
// { dg-do compile { target c++11 } }

struct a {
  ~a() = delete;
};
struct b {
  ~b() {}
  union {
    a c;
  };
};
