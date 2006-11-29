//PR c++/29022

struct A
{
  operator int();
};

struct B : virtual A;	// { dg-error "token" }

int foo(B &b)
{
  return b;		// { dg-error "cannot convert" }
}
