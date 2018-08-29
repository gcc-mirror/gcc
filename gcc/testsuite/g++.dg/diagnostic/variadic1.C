// PR c++/55241
// { dg-do compile { target c++11 } }

template<int N> struct B { };
template<typename... T> struct A
{
  B<sizeof...(T)> f();		// { dg-message "sizeof\\.\\.\\." }
  B<42> f();			// { dg-error "cannot be overloaded" }
};
