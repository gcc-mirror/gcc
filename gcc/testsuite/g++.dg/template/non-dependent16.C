// This test verifies that after resolving a non-dependent call expression
// ahead of time, we prune all but the selected candidate from the overload
// set.  Without this optimization, overload resolution for the final call to
// f<void>() would be exponential in the size of the overload set.

// { dg-do compile { target c++11 } }

template<class T> void f();
template<class T> auto f() -> decltype(f<void>(), 1, *T());
template<class T> auto f() -> decltype(f<void>(), 2, *T());
template<class T> auto f() -> decltype(f<void>(), 3, *T());
template<class T> auto f() -> decltype(f<void>(), 4, *T());
template<class T> auto f() -> decltype(f<void>(), 5, *T());
template<class T> auto f() -> decltype(f<void>(), 6, *T());
template<class T> auto f() -> decltype(f<void>(), 7, *T());
template<class T> auto f() -> decltype(f<void>(), 8, *T());
template<class T> auto f() -> decltype(f<void>(), 9, *T());
template<class T> auto f() -> decltype(f<void>(), 10, *T());
template<class T> auto f() -> decltype(f<void>(), 11, *T());
template<class T> auto f() -> decltype(f<void>(), 12, *T());
template<class T> auto f() -> decltype(f<void>(), 13, *T());
template<class T> auto f() -> decltype(f<void>(), 14, *T());
template<class T> auto f() -> decltype(f<void>(), 15, *T());
template<class T> auto f() -> decltype(f<void>(), 16, *T());
template<class T> auto f() -> decltype(f<void>(), 17, *T());
template<class T> auto f() -> decltype(f<void>(), 18, *T());
template<class T> auto f() -> decltype(f<void>(), 19, *T());
template<class T> auto f() -> decltype(f<void>(), 20, *T());
template<class T> auto f() -> decltype(f<void>(), 21, *T());
template<class T> auto f() -> decltype(f<void>(), 22, *T());
template<class T> auto f() -> decltype(f<void>(), 23, *T());
template<class T> auto f() -> decltype(f<void>(), 24, *T());
template<class T> auto f() -> decltype(f<void>(), 25, *T());

int main() {
  f<void>();
}
