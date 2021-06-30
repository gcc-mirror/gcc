// An assumed contract shouldn't break constant evaluation.

// { dg-do compile { target c++20 } }
// { dg-additional-options -fcontracts }

bool b;

constexpr int f() [[ pre assume: b ]] { return 42; }

static_assert (f() > 0);
