// PR c++/111272
// { dg-do compile { target c++14 } }
// { dg-options "-Werror=invalid-constexpr -fno-implicit-constexpr" }
// { dg-prune-output "some warnings being treated as errors" }

struct Jam
{
  // constexpr  // n.b.
  int ft() { return 42; } // { dg-message "declared here" }

  constexpr Jam() { ft(); } // { dg-error "call to non-.constexpr. function" }
// { dg-message "declared here" "" { target c++20_down } .-1 }
};

constexpr bool test()
{
  Jam j; // { dg-error "called in a constant expression" }
  return true;
}

static_assert(test(), ""); // { dg-error "non-constant condition" }
