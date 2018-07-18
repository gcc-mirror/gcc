// PR c++/78906
// { dg-do compile { target c++14 } }

template <typename> struct A { static constexpr int digits = 0; };
template <typename> struct B {
  template <int, typename MaskInt = int, int = A<MaskInt>::digits>
  static constexpr int XBitMask = 0;
};
struct C {
  using ReferenceHost = B<int>;
  template <int> static decltype(ReferenceHost::XBitMask<0>) XBitMask;
};
int main() { C::XBitMask<0>; }
