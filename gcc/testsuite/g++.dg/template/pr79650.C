// PR c++/79650
// { dg-do compile { target c++11 } }
// { dg-options "" }

typedef __INTPTR_TYPE__ intptr_t;
template<intptr_t> struct A {};

void
foo ()
{
  static int a, b;
lab1:
lab2:
  A<(intptr_t)&&lab1 - (__INTPTR_TYPE__)&&lab2> c;	// { dg-error "conversion from pointer type" }
  A<(intptr_t)&&lab1 - (__INTPTR_TYPE__)&&lab1> d;	// { dg-error "conversion from pointer type" }
  A<(intptr_t)&a - (intptr_t)&b> e;			// { dg-error "conversion from pointer type" }
  A<(intptr_t)&a - (intptr_t)&a> f;			// { dg-error "conversion from pointer type" }
  A<(intptr_t)sizeof(a) + (intptr_t)&a> g;		// { dg-error "conversion from pointer type" }
  A<(intptr_t)&a> h;					// { dg-error "conversion from pointer type" }
}
