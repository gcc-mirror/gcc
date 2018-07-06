/* PR c++/29727 */
/* { dg-do compile } */

template<int> struct A
{
  static int a[1];
};
template<int N> int A<N>::a[1] = { X:0 }; /* { dg-error "does not allow GNU designated|was not declared|designated initializer for an array" } */

void foo()
{
  A<0>::a;
}


