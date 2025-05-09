// PR c++/120204
// { dg-do compile { target c++14 } }

template<class T, int t>
struct array{};

template <typename... TArgs> struct ILEArglist {
  using Sizes = array<int, sizeof...(TArgs)>;
  static constexpr int size() {	// { dg-bogus "not usable" }
    Sizes &offsets_c = offsets;	// { dg-error "depends on itself" }
    return 0;
  }
  array<char, size()> offsets(); // { dg-error "constant expression" }
};
auto arglist = ILEArglist<>();
