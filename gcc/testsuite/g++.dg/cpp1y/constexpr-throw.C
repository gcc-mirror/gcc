// { dg-do compile { target c++14 } }

constexpr void f1() {
  if (false)
    throw;
}

constexpr void f2() {
  if (true)
    throw;	// { dg-error "not a constant expression" }
}

constexpr void f3() {
  if (false)
    ;
  else
    throw;	// { dg-error "not a constant expression" }
}

constexpr void f4() {
  throw;	// { dg-error "not a constant expression" }
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
static_assert(fun(1), ""); // { dg-error "non-constant" }
