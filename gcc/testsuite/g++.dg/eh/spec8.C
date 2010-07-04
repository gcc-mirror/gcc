// PR c++/24817

struct exception {};

template <typename T> void foo() throw(exception); // { dg-error "declaration" }
template <typename T> void foo(); // { dg-error "exception" }

struct bar
{
  template <typename T> friend void foo(); // { dg-error "exception" }
};
