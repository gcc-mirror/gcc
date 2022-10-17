// Test that we don't uselessly instantiate f<A> immediately while parsing g.
// Doing so is permitted by the standard, but has no benefit and breaks code
// unnecessarily.

// { dg-do compile { target c++11 } }

// -O activates the call to maybe_constant_value in cp_fold.
// { dg-additional-options -O }

template <class T>
constexpr int f (const T* p) { return p->i; }

constexpr int g(const struct A* p) { return f(p); }

struct A { int i; };

// Instantiating f<A> at EOF works fine.
