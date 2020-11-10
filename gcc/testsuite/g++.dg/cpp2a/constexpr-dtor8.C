// PR c++/97388
// { dg-do compile { target c++20 } }

struct S {
  int *s;
  constexpr S () : s(new int) {}
  S (const S &) = delete;
  S &operator= (const S &) = delete;
  constexpr ~S () { delete s; }	// { dg-error "already deallocated" }
};

constexpr bool
foo (S v)
{
  delete v.s;
  return true;
}

static_assert (foo (S ()));	// { dg-error "non-constant condition for static assertion" }
