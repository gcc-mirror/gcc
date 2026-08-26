// PR c++/126828
// { dg-do compile { target c++11 } }

template <int> class a{};
template <int b> using c = a<b>;
template <int> void d() {
  constexpr unsigned blocksize = 8;
  [&] {
    c<blocksize> g;
  };
}
