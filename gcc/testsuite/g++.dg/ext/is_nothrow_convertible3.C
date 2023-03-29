// PR c++/106784
// { dg-do compile { target c++11 } }
// Make sure we don't reject this at runtime by trying to instantiate
// something that would be ill-formed.

struct A;
struct B { template<class T> B(const T&) noexcept { T::nonexistent; } };

static_assert(__is_nothrow_convertible(const A&, B), "");
