// PR c++/118053
// { dg-do compile { target c++14 } }
// { dg-additional-options -O }

template <typename _Tp> struct vector {
  _Tp * _M_finish;
  vector(_Tp);
  vector(const vector &);
  constexpr auto back() { return *_M_finish; }
};
template <typename Funct> void
run(Funct funct) { funct(1); }

vector<int>
runner() try {
  vector<int> vec{1};
  run([&](auto) { vec.back(); });
  return vec;
} catch (...) {
  return 1;
}
