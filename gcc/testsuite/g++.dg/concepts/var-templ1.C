// PR c++/67117
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <class T>
  requires false
constexpr bool v = true;

template <class T>
constexpr bool f() { return true; }

template <class T>
  requires v<T>
constexpr bool f() { return false; }

static_assert(f<void>());
static_assert(v<void>);		// { dg-error "invalid" }
