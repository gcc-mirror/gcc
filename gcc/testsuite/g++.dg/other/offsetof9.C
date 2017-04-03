// PR c++/68727
// { dg-do compile }
// { dg-options "-Winvalid-offsetof" }

struct A { int i; };
struct B : virtual A { };
__SIZE_TYPE__ s = __builtin_offsetof (B, A::i);	// { dg-warning "offsetof within non-standard-layout type" }

template <typename T>
__SIZE_TYPE__
foo ()
{
  return __builtin_offsetof (T, A::i)		// { dg-warning "offsetof within non-standard-layout type" }
	 + __builtin_offsetof (B, A::i);	// { dg-warning "offsetof within non-standard-layout type" }
}

__SIZE_TYPE__ t = foo<B> ();
