// { dg-do compile }

template <typename> struct d {
  template <typename e> d(e);
};
template <> template <typename e> d<int>::d(e);
template <> template <typename e> d<int>::d(e) {
  long g;
  (void)g;
}
template d<int>::d(char);
