// { dg-options "-Wabi=9 -O0" }

struct A { };

void f(A, A) { }		// No warning, trailing parms all empty
void f(A, A, int) { }		// { dg-warning "ABI" }
__attribute__ ((always_inline))
inline void f(A a, int i) { }	// No warning, always inlined
__attribute__ ((always_inline))
inline void f2(A a, int i)	// But the call within the fn gets a warning
{				// when it's inlined into main
  f(a,a,i);			// { dg-warning "ABI" }
}
inline void f3(A a, int i)	// This one is never called
{
  f(a,a,i);
}
int main()
{
  A a;
  f(a,a);
  f(a,a,42);			// { dg-warning "ABI" }
  f(a,42);
  f2(a,42);
}
