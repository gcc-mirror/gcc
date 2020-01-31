// { dg-do compile { target c++14 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

constexpr int *f4(bool b) {
  if (b) {
    return nullptr;
  } else {
    return new int{42}; // { dg-error "call to non-.constexpr." "" { target c++17_down } }
  }			// { dg-error "is not a constant expression because allocated storage has not been deallocated" "" { target c++2a } .-1 }
}
static_assert(f4(true) == nullptr, "");
static_assert(f4(false) == nullptr, ""); // { dg-error "non-.constant. condition|" }
