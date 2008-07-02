// Test for initializer-list 'explicit' rule
// { dg-options "-std=c++0x" }

struct A
{
  explicit A(int,int);
  operator bool();
};

A f(A)
{
  A{1,2};
  A a1{1,2};
  new A{1,2};
  if (A a5{1,2});

  A({1,2});			// { dg-error "explicit" }
  A a2({1,2});			// { dg-error "explicit" }
  A a3 = {1,2};			// { dg-error "explicit" }
  new A({1,2});			// { dg-error "explicit" }
  f({1,2});			// { dg-error "explicit" }
  a1 = {1,2};			// { dg-error "explicit" }
  if (A a4 = {1,2});		// { dg-error "explicit" }
  return {1,2};			// { dg-error "explicit" }
}

struct B
{
  A a;
  B(): a{1,2} {}
  B(const B&): a({1,2}) {}	// { dg-error "explicit" }
};
