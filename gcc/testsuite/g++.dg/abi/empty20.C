// PR c++/60336
// { dg-options "-Wabi=11 -O0" }

struct A { };

void f(A, A) { }	// No warning, trailing parms all empty
void f(A, A, int) { }	// { dg-warning "ABI" "" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } }
__attribute__ ((always_inline))
inline void f(A a, int i) // No warning, always inlined
{
  f(a,a,i); // { dg-warning "ABI" "" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } }
}
int main()
{
  A a;
  f(a,a);
  f(a,a,42); // { dg-warning "ABI" "" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } }
  f(a,42);
}
