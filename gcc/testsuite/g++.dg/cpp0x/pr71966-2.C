// PR c++/71966
// { dg-do compile { target c++11 } }

struct A
{ 
  constexpr A (int);  // { dg-message "never defined" }
  constexpr operator int () const { return 0; }
  int c;
};

template <int>
struct B {};

constexpr A a = 0;  // { dg-error "before its definition" }
B<a> b;
