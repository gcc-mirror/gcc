// PR c++/103299
// { dg-do compile { target c++20 } }

struct foo {
  union {
    int fp1{};
    char fp2;
  };
};

static_assert(foo{.fp2={}}.fp2 == 0);
