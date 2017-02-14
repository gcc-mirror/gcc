// In C++11 explicit instantiation without a nested-name-specifier must be in
// the same namespace.

namespace N {
  template <class T> class foo {};
  template <class T> class bar {};
}

using N::bar;
template class bar<int>;	// { dg-error "" "" { target c++11 } }

using namespace N;
template class foo<int>;	// { dg-error "" "" { target c++11 } }
