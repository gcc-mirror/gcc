// PR c++/102553
// { dg-do compile { target c++20 } }

struct s1{};
template<int> inline constexpr s1 ch{};

template<s1 f> struct s2{};
template<s1 f> using alias1 = s2<f>;

template<class T>
void general(int n) {
  alias1<ch<1>>{};
}

template void general<int>(int);
