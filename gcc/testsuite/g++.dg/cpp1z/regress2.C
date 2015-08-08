// PR c++/67142
// { dg-options -std=c++1z }

namespace detail {
template <int> int split_at;
}
struct A {
  decltype(0) operator()();
};
template <typename> A make;
struct Tuple;
auto check =
    [](auto, auto, auto) { [](auto... xs) { [=] { make<Tuple>(xs...); }; }(); };
int main() {
  namespace vd = detail;
  check(vd::split_at<0>, make<Tuple>, make<Tuple>);
}
