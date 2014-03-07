// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }

template <typename> class S {};
extern template class S<int>;
