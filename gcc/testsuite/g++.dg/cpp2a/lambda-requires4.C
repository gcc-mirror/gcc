// PR c++/106976
// { dg-do compile { target c++20 } }

struct S{
  constexpr static auto s = requires { []; }; // { dg-error "expected '\{'" }
};
