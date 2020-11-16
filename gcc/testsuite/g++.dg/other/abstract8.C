// P0929R2: Checking for abstract class types.
// { dg-do compile { target c++11 } }
// { dg-additional-options -Wno-return-type }

struct A
{
 virtual void f() = 0;
};

struct B
{
  A a;				// { dg-error "abstract" }
  A ar[4];			// { dg-error "abstract" }
};

using Aa = A[4];		// OK
Aa* aap;			// OK

extern A a;			// OK
extern Aa aa;			// OK
A f();				// OK
void g(A);			// OK

A a;				// { dg-error "abstract" }
Aa aa;				// { dg-error "abstract" }
A f() { }			// { dg-error "abstract" }
void g(A) { }			// { dg-error "abstract" }

int main()
{
  (A(a));			// { dg-error "abstract" }
  A{};				// { dg-error "abstract" }
  static_cast<A>(a);		// { dg-error "abstract" }
  Aa{};				// { dg-error "abstract" }
  f();				// { dg-error "abstract" }
  decltype(f())* p;		// OK
  g(a);				// { dg-error "abstract" }

  throw a;			// { dg-error "abstract" }
}
