// PR c++/99869
// { dg-do compile { target c++20 } }

template <class T, class U> concept same_as = __is_same(T, U);

template <class>
void f() {
  for (same_as<int> auto c : "") {} // { dg-error "constraint" }
  for (same_as<char> auto c : "") {}
}
