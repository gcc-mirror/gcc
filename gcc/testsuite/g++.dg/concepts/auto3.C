// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <class...> class tuple {};

tuple<int> t;
tuple<auto> y = t;		// { dg-error "invalid|cannot convert" }

tuple<int,double> t2;
tuple<auto...> x = t2;		// { dg-error "invalid|cannot convert" }
tuple<auto...> x2 = t;		// { dg-error "invalid|cannot convert" }

tuple<auto> y2 = t2;		// { dg-error "invalid|cannot convert" }
