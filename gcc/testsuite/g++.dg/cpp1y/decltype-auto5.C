// PR c++/102229
// { dg-do compile { target c++14 } }

struct S {
  constexpr static decltype(auto) x = 42;
  const constexpr static decltype(auto) y = 42; // { dg-error "cannot be cv-qualified" }

  constexpr decltype(auto) mfn1 () { return 0; }
  const constexpr decltype(auto) mfn2 () { return 0; } // { dg-error "cannot be cv-qualified" }
};

constexpr decltype(auto) i = 42;
const constexpr decltype(auto) j = 42; // { dg-error "cannot be cv-qualified" }

constexpr decltype(auto) fn() { return 42; }
const decltype(auto) fn2() { return 42; } // { dg-error "cannot be cv-qualified" }

auto constexpr foo() -> const decltype(auto) // { dg-error "cannot be cv-qualified" }
{
  return 0;
}

#if __cpp_concepts
template<typename>
concept C = true;

constexpr C decltype(auto) x1 = 0;
const constexpr C decltype(auto) x2 = 0; // { dg-error "cannot be cv-qualified" "" { target c++20 } }

constexpr C decltype(auto) fn3() { return 0; }
const constexpr C decltype(auto) fn4() { return 0; } // { dg-error "cannot be cv-qualified" "" { target c++20 } }
#endif

template<const decltype(auto) = 42> // { dg-error "cannot be cv-qualified" }
void g ();
