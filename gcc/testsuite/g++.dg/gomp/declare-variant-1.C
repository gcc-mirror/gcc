struct S
{
  void foo ();
  void bar (const S &x);
#if __cplusplus >= 201103L
  S &baz (const S &x);
  S &qux (S &&x);
#endif
  void quux (int x);
  #pragma omp declare variant (foo) match (user={condition(0)})	// { dg-error "'declare variant' on constructor" }
  S ();
  #pragma omp declare variant (foo) match (user={condition(0)})	// { dg-error "'declare variant' on destructor" }
  ~S ();
  #pragma omp declare variant (bar) match (user={condition(0)})	// { dg-error "'declare variant' on constructor" }
  S (const S &x);
  #pragma omp declare variant (quux) match (user={condition(0)})	// { dg-error "'declare variant' on constructor" }
  S (int x);
#if __cplusplus >= 201103L
  #pragma omp declare variant (baz) match (user={condition(0)})	// { dg-error "'declare variant' on defaulted" "" { target c++11 } }
  S &operator= (const S &x) = default;
  #pragma omp declare variant (qux) match (user={condition(0)})	// { dg-error "'declare variant' on deleted" "" { target c++11 } }
  S &operator= (S &&) = delete;
#endif
  int s;
};
void corge (int);
#pragma omp declare variant (corge) match (user={condition(0)})
void grault (int x);
