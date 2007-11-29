// PR c++/34268
// { dg-do compile }

struct A
{
  __decltype (A);	// { dg-error "must be an expression" }
  __decltype (~A);	// { dg-error "must be an expression" }
};

struct B
{
  __typeof__ (B);
  __typeof__ (~B);	// { dg-error "expected primary-expression" }
};
