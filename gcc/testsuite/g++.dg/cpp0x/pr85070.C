// { dg-do compile { target c++11 } }

struct A;

struct B
{
  constexpr A & operator= (const A &);  // { dg-warning "used" "" { target c++14_only } }
};

struct A : B  // { dg-error "cannot be overloaded" "" { target c++14_only } }
{
  using B::operator=;
} a { a = a };
