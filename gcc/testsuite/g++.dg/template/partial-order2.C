// CWG 2235

namespace X
{
  template<typename T> struct Const { typedef void type; };
  template<typename T> void f(T, typename Const<T>::type*); // T1
  template<typename T> void f(T, void *); // T2
  void g(void *p) { f(0, p); }
}

namespace Y
{
  struct A { A(int); };
  struct B { B(int); };

  template<typename T> void f(T, A);
  template<typename T> void f(T*, B);

  void g(int *p) { f(p, 0); }	// { dg-error "ambiguous" }
}
