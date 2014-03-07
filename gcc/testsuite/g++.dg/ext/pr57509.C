/* { dg-do compile { target c++11 } } */

template <bool> struct enable_if {};
template <> struct enable_if<true> {typedef void type;};
template <class T> void f (T& v) { v = __builtin_shuffle (v, v); }
template <class T> void g (T const&) {}
template <class T> auto g (T const& x) -> typename enable_if<sizeof(__builtin_shuffle(x,x))!=2>::type {}
typedef int v4i __attribute__((vector_size(4*sizeof(int))));
typedef float v4f __attribute__((vector_size(4*sizeof(float))));
int main(){
  v4i a = {1,2,3,0};
  f(a);
  v4f b = {1,2,3,0};
  g(b);
}
