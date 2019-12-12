// { dg-do compile { target c++11 } }

struct A;

struct B
{
  constexpr A & operator= (const A &);  // { dg-warning "used" "" { target c++14 } }
};

struct A : B  // { dg-error "cannot be overloaded" "" { target c++14 } }
{
  using B::operator=;
} a { a = a };
