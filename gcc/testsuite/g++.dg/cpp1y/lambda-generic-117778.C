// PR c++/117778
// { dg-do compile { target c++14 } }

auto l1 = [](auto (*fp)() -> auto) { return fp; };
auto l2 = [](auto fp() -> auto) { return fp; };
auto l3 = [](auto fp()) { return fp; };
auto l4 = [](auto (*fp)()) { return fp; };
auto l5 = [](auto fp() -> auto) -> auto { return fp; };
auto l6 = [](auto fp(auto fp2()) -> auto) -> auto { return fp; }; // { dg-error ".auto. parameter not permitted" }
auto l7 = [](auto fp(auto fp2() -> auto) -> auto) -> auto { return fp; }; // { dg-error ".auto. parameter not permitted" }
auto l8 = [](int fp(auto fp2())) { return fp; }; // { dg-error ".auto. parameter not permitted" }
auto l9 = [](auto fp(auto fp2() -> auto) -> auto) { return fp; }; // { dg-error ".auto. parameter not permitted" }
