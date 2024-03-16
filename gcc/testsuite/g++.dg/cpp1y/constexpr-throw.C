// { dg-do compile { target c++14 } }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

constexpr void f1() {
  if (false)
    throw;
}

constexpr void f2() {
  if (true)
    throw;	// { dg-error "not a constant expression" "" { target c++20_down } }
}

constexpr void f3() {
  if (false)
    ;
  else
    throw;	// { dg-error "not a constant expression" "" { target c++20_down } }
}

constexpr void f4() {
  throw;	// { dg-error "not a constant expression" "" { target c++20_down } }
}

constexpr int fun(int n) {
  switch (n) {
  case 0:
    return 1;
  default:
    throw; // { dg-error "not a constant expression" }
  }
}

static_assert(fun(0), "");
static_assert(fun(1), ""); // { dg-error "non-constant|in .constexpr. expansion of" }
