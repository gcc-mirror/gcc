// PR c++/97388
// { dg-do compile { target c++20 } }

struct S {
  int *s;
  constexpr S () : s(new int) {}	// { dg-error "is not a constant expression because allocated storage has not been deallocated" }
  S (const S &) = delete;
  S &operator= (const S &) = delete;
  constexpr ~S () { delete s; }
};

constexpr bool
foo (S v)
{
  v.s = nullptr;
  return true;
}

static_assert (foo (S ()));	// { dg-error "non-constant condition for static assertion" }
