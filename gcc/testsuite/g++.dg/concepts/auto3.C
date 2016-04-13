// { dg-options "-std=c++1z -fconcepts" }

template <class...> class tuple {};

tuple<int> t;
tuple<auto> y = t;

tuple<int,double> t2;
tuple<auto...> x = t2;
tuple<auto...> x2 = t;

tuple<auto> y2 = t2;		// { dg-error "" }
