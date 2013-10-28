// { dg-options -std=c++11 }
// We decided in Rapperswil that it's OK if any value of decide can produce
// a constant expression.

constexpr int may_throw(bool decide) {
  return decide ? 42 : throw -1;
}
