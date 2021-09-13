// PR c++/99201
// { dg-do compile { target c++17 } }

template <typename RefF>
  auto
  make_tester(const RefF& reffun)
  {
    return [=](auto in) {
      auto&& expected = [&](const auto&... vs) {
        if constexpr (sizeof(in) > 0)
          return [&](int i) { return reffun(vs[i]...); }(0);
        else
          return [&](int i) { return reffun(vs[i]...); }(0);
      };
    };
  }

int main()
{
  make_tester([](int x) { return x; })(0);
  return 0;
}
