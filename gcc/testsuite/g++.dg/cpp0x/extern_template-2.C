// { dg-options "-std=c++11 -pedantic" }

template <typename> class S {};
extern template class S<int>;
