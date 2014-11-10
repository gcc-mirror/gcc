// DR 2007
// We shouldn't instantiate A<void> to lookup operator=, since operator=
// must be a non-static member function.

template<typename T> struct A { typename T::error e; };
template<typename T> struct B { };
B<A<void> > b1, &b2 = (b1 = b1);
