// PR c++/19878

struct S {
  char k;
};
char const volatile S::* const p01 = &S::k;

