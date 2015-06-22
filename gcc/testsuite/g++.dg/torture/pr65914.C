/* { dg-do compile } */
/* { dg-additional-options "-std=c++14" } */

enum expression_template_option { et_on };
template <class, expression_template_option = et_on> class A;
template <class, class, class, class = void, class = void> struct expression;
template <class T> struct B { typedef const T &type; };
template <class tag, class A1, class A2, class A3, class A4>
struct B<expression<tag, A1, A2, A3, A4>> {
  typedef expression<tag, A1, A2> type;
};
template <class tag, class Arg1, class Arg2>
struct expression<tag, Arg1, Arg2> {
  expression(Arg1 p1, const Arg2 &p2) : arg1(p1), arg2(p2) {}
  typename B<Arg1>::type arg1;
  typename B<Arg2>::type arg2;
};
template <class Backend> expression<int, int, A<Backend>> sin(A<Backend>) {
  return expression<int, int, A<Backend>>(0, 0);
}
template <class tag, class A1, class A2, class A3, class A4>
expression<int, int, expression<tag, A1, A2>>
  asin(expression<tag, A1, A2, A3, A4> p1) {
  return expression<int, int, expression<tag, A1, A2>>(0, p1);
}
template <class B, expression_template_option ET, class tag, class Arg1,
	  class Arg2, class Arg3, class Arg4>
expression<int, A<B>, expression<tag, Arg1, Arg2>>
  operator+(A<B, ET>, expression<tag, Arg1, Arg2, Arg3, Arg4> p2) {
  return expression<int, A<B>, expression<tag, Arg1, Arg2>>(0, p2);
}
template <class tag, class Arg1, class Arg2, class Arg3, class Arg4, class tag2,
	  class Arg1b, class Arg2b, class Arg3b, class Arg4b>
expression<int, expression<tag, Arg1, Arg2>, expression<tag2, Arg1b, Arg2b>>
  operator*(expression<tag, Arg1, Arg2, Arg3, Arg4> p1,
	    expression<tag2, Arg1b, Arg2b, Arg3b, Arg4b> p2) {
  return expression<int, expression<tag, Arg1, Arg2>,
		    expression<tag2, Arg1b, Arg2b>>(p1, p2);
}
template <class B> expression<int, A<B>, A<B>> operator/(A<B>, A<B>) {
  return expression<int, A<B>, A<B>>(0, 0);
}
template <class tag, class Arg1, class Arg2, class Arg3, class Arg4, class V>
void operator/(expression<tag, Arg1, Arg2, Arg3, Arg4>, V);
template <class, expression_template_option> class A {
public:
  A() {}
  template <class V> A(V) {}
};
template <class T, class Policy> void jacobi_recurse(T, T, Policy) {
  T a, b, c;
  (a+asin(b/c) * sin(a)) / 0.1;
}
template <class T, class Policy> void jacobi_imp(T p1, Policy) {
  T x;
  jacobi_recurse(x, p1, 0);
}
template <class T, class U, class V, class Policy>
void jacobi_elliptic(T, U, V, Policy) {
  jacobi_imp(static_cast<T>(0), 0);
}
template <class U, class T, class Policy> void jacobi_sn(U, T, Policy) {
  jacobi_elliptic(static_cast<T>(0), 0, 0, 0);
}
template <class U, class T> void jacobi_sn(U, T p2) { jacobi_sn(0, p2, 0); }
template <class T> void test_extra(T) {
  T d;
  jacobi_sn(0, d);
}
void foo() { test_extra(A<int>()); }
