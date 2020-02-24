// PR c++/68061
// { dg-do compile { target c++11 } }

template <class T>
  requires true			// { dg-error "requires" "" { target { ! concepts } } }
[[deprecated]] void f(T);
