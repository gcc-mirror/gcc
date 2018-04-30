struct duration {
  long val;
  static constexpr duration max() { return {}; }
};
struct S {
  duration max = duration::max();
};
void Ice(S& s) {
  s = {};
}
