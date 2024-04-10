/* { dg-options "-Wstrict-aliasing" } */
/* PR c++/97990 */
/* This used to crash with lto and strict aliasing enabled as the
   vector type variant still had TYPE_ALIAS_SET set on it. */

typedef __attribute__((__vector_size__(sizeof(short)))) short TSimd;
TSimd hh(int);
struct y6
{
  TSimd VALUE;
  ~y6();
};
template <class T1,class T2>
auto f2(T1 p1, T2){
  return hh(p1) <= 0;
}
void f1(){
  f2(0, y6{});
}
