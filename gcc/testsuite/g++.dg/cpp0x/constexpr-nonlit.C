// FIXME this is currently invalid, but seems like it should be OK
// { dg-do compile { target c++11 } }

struct A { A() { } };

template<class T>
constexpr bool ignore(T&&) { return true; }

static_assert(ignore(10), "Error"); // OK

A s;

static_assert(ignore(s), "Error"); // Currently an error
