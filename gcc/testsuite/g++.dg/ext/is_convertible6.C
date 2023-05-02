// PR c++/109680
// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

SA(!__is_convertible(int () const, int (*)()));
SA(!__is_convertible(int (*)(), int () const));

SA( __is_convertible(int (), int (*)()));
SA(!__is_convertible(int (*)(), int ()));

SA( __is_convertible(int (int), int (*) (int)));
SA(!__is_convertible(int (*) (int), int (int)));

SA(!__is_convertible(int (int) const, int (*) (int)));
SA(!__is_convertible(int (*) (int), int (int) const));
