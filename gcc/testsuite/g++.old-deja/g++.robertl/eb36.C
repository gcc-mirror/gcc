// Build don't link:

#include <vector>

template <typename T=float> class foo {
public:
  foo();
  foo(vector<int> v);
private:
  vector<int> v;
  T t;
};

template <typename T>
foo<T>::foo()               :v(),   t() {}
template <typename T=float>	// ERROR - default parm for member template XFAIL *-*-*
foo<T>::foo(vector<int> v_) :v(v_), t() {}

foo<float> a;
