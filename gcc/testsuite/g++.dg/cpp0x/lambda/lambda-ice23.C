// PR c++/59991
// { dg-do compile { target c++11 } }

template <typename T>
constexpr int r(T x) {
  auto f = [r,x]() { return r(x); }; // { dg-error "13:capture of non-variable" }
  return 0;
}

int main()
{
  r(0);
}
