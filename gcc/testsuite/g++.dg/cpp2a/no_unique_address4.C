// PR c++/96105
// { dg-do compile { target c++20 } }

struct Empty {};

struct A {
  Empty emp [[no_unique_address]][3];
};

struct B : A {
  float f;
};

struct C {
  Empty emp [[no_unique_address]][3];
  float f;
};

extern char szc[sizeof(C)];
extern char szc[sizeof(float) * 2];  // GCC likes this
extern char szb[sizeof(B)];
extern char szb[sizeof(float) * 2];  // GCC does not like this
