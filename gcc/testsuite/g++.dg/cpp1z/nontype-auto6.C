// { dg-do compile { target c++11 } }

template <int N> struct A;
template <typename T, T N> int foo(A<N> *) = delete;
void foo(void *);
void bar(A<0> *p) {
  foo(p);			// { dg-error "" "" { target c++17 } }
}
