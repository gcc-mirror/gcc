// { dg-options "-std=c++17 -fconcepts" }

template <class T1, class T2> class A { };

A<int, int> a;
A<double, float> a2;
A<double, double> a22;

A<auto, auto> b = a;
A<auto, auto> b1 = a2;

template <class T> concept bool C = __is_same_as (T, int);

A<C,C> b2 = a;
A<C,C> b3 = a2;			// { dg-error "" }
A<C,C> b32 = a22;		// { dg-error "" }

template <class T> concept bool C2() { return __is_enum (T); }

enum E1 { };
enum E2 { };

A<E1,E1> a3;
A<C2,C2> b4 = a3;

A<E1,E2> a4;
A<C2,C2> b5 = a4;		// { dg-error "" }
