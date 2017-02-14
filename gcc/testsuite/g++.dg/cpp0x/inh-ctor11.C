// { dg-do compile { target c++11 } }
// { dg-options -fno-new-inheriting-ctors }

struct A
{
  A(int, ...);			// { dg-message "declared here" }
};

struct B: A
{
  using A::A;			// { dg-warning "ellipsis" }
};

B b1(42);
B b2(42, 1.0);			// { dg-error "no match" }
