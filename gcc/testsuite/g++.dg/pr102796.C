// { dg-do compile }
// { dg-options "-O3 -fno-tree-ccp -fno-tree-fre -fno-tree-forwprop -std=c++17" }

namespace std {
template <class _E>
struct initializer_list {
  const int* __begin_;
  decltype(sizeof(int)) __size_;
};
}  // namespace std
struct destroyme1 {};
struct witharg1 {
  witharg1(const destroyme1&);
  ~witharg1();
};
std::initializer_list globalInitList2 = {witharg1(destroyme1()),
                                         witharg1(destroyme1())};

