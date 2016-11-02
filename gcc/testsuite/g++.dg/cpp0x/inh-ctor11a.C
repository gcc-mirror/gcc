// { dg-do compile { target c++11 } }
// { dg-options -fnew-inheriting-ctors }

struct A
{
  A(int, ...);
};

struct B: A
{
  using A::A;
};

B b1(42);
B b2(42, 1.0);			// { dg-bogus "ellipsis" "" { xfail *-*-* } }
