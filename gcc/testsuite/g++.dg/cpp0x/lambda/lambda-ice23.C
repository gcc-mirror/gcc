// PR c++/59991
// { dg-do compile { target c++11 } }

template <typename T>
constexpr int r(T x) {
  auto f = [r,x]() { return r(x); }; // { dg-error "incomplete type" }
  return 0;
}

int main()
{
  r(0);
}
