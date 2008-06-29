// { dg-options "-std=c++0x -pedantic" }

template <typename> class S {};
extern template class S<int>;
