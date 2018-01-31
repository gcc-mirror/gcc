// PR c++/84092
// { dg-do compile { target c++14 } }

template < typename T > int a (T::template b);
