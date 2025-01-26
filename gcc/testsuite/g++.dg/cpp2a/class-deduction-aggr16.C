// PR c++/118528
// { dg-do compile { target c++20 } }

template<class T>
struct E { T t[130][2]; };

#define P 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
#define Q { 1, 2 }, { 3, 4 }, { 5, 6 }, { 7, 8 }, { 9, 10 }, { 11, 12 }, \
	  { 13, 14 }, { 15, 16 }
E e1 { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, 1, 2, 3, 4 };
E e2 { { Q, Q, Q, Q, Q, Q, Q, Q, Q, Q, Q, Q, Q, Q, Q, Q, { 1, 2 }, { 3, 4 } } };

template<class T>
struct F { T t[2][130]; };

F f1 { P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, P, 1, 2, 3, 4 };
F f2 { { { P, P, P, P, P, P, P, P, 1, 2 }, { P, P, P, P, P, P, P, P, 3, 4 } } };
