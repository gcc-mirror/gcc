// PR c++/65973
// { dg-do compile { target c++14 } }

class foo {
  constexpr foo() noexcept { __func__; };
};
