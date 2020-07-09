// PR c++/96052
// { dg-do compile { target c++20 } }

struct Q {
  struct {
  } emp alignas(8) [[no_unique_address]];
  char x;
};
struct QQ {
  char x;
  Q q;
};

struct Z {
  char x alignas(8) [[no_unique_address]];
};
struct ZZ {
  char x;
  Z z;
};

extern char qx[sizeof(QQ)];
extern char qx[16];
extern char qz[sizeof(ZZ)];
extern char qz[16];
