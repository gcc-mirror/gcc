//PR c++/28737

template<void> struct A;                // { dg-error "not a valid type" }

template<typename> struct B;

template<void N> struct B<A<N> > {};   // { dg-error "not a valid type|declared|invalid" }
