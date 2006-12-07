namespace A
{
  typedef int T;	// { dg-error "previous declaration" }
}

class A::T x;		// { dg-error "using typedef-name|invalid type" }
