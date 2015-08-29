// { dg-do compile { target c++14 } }

constexpr int *f4(bool b) {
  if (b) {
    return nullptr;
  } else {
    return new int{42}; // { dg-error "call to non-constexpr" }
  }
}
static_assert(f4(true) == nullptr, "");
static_assert(f4(false) == nullptr, ""); // { dg-error "non-constant condition" }
