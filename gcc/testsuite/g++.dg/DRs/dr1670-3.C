// DR 1670 - auto as conversion-type-id
// { dg-do compile { target c++14 } }
// { dg-options "" }

struct S {
  operator auto () { return 0; }
};
struct T {
  operator decltype (auto) () { return 0; }
};
