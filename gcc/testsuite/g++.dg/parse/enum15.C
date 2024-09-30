// PR c++/115806
// { dg-do compile }

template <typename T>
struct S {
  enum E { a }; // { dg-note "previous definition" }
  enum E { b }; // { dg-error "multiple definition" }
};
S<int> s;
