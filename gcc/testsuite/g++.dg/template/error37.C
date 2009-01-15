// { dg-do compile }
// PR C++/29388
// We used to ICE in is_ancestor because we would use int as the context of foo
// but that is invalid.

template<int> struct A
{
	  typedef int T;
	    void foo();
};

template<int N> void A<N>::T::foo() {} // { dg-error "" }
