/* PR c++/67 */
/* { dg-do compile } */

template <class T> struct foo {
   static const int bar [3];
};
// Used to fail if 2+1 rather than 3.
template <class T> const int foo<T>::bar [2+1] = { 0, 0, 0 };
