// PR c++/18161
// { dg-options "" }

template <class T> struct Y;
template <> struct Y<bool> {};

template <typename T = typeof (1 == 1)> struct X { Y<T> a; };
template struct X <>; 
