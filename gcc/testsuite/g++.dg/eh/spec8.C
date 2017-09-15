// PR c++/24817
// { dg-do compile { target c++14_down } }

struct exception {};

template <typename T> void foo() throw(exception); // { dg-message "declaration" }
						   // { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } .-1 }
template <typename T> void foo(); // { dg-error "exception" }

struct bar
{
  template <typename T> friend void foo(); // { dg-error "exception" }
};
