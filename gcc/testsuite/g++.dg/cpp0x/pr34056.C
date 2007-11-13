// PR c++/34056
// { dg-do compile }
// { dg-options "-std=c++0x" }

template<typename... T> struct A
{
  void foo (T *) { ++p; }	// { dg-error "not expanded|T" }
  void bar (T **) { }		// { dg-error "not expanded|T" }
  T *p;				// { dg-error "not expanded|T" }
};
