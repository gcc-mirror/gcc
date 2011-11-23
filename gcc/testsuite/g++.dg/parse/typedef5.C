namespace A
{
  typedef int T;	// { dg-message "previous declaration" }
}

class A::T x;		// { dg-error "using typedef-name|invalid type" }
