// { dg-do compile }
// { dg-options "-O2 -w -std=c++11" }

using int32 = int;
int ShortestPath_distance;
struct FloatWeightTpl {
  FloatWeightTpl(float f) : value_(f) {}
  float Value() { return value_; }
  float value_;
};
template <class T> bool operator!=(FloatWeightTpl w1, T w2) {
  bool __trans_tmp_2;
  FloatWeightTpl __trans_tmp_3 = w1;
  __trans_tmp_2 = __trans_tmp_3.Value() == w2.Value();
  return __trans_tmp_2;
}
struct TropicalWeightTpl : FloatWeightTpl {
  TropicalWeightTpl(float f) : FloatWeightTpl(f) {}
  static TropicalWeightTpl Zero();
  static TropicalWeightTpl NoWeight() {
    float __trans_tmp_5 = __builtin_nanf("");
    return __trans_tmp_5;
  }
  bool Member() { return value_; }
};
TropicalWeightTpl Plus(TropicalWeightTpl w1, TropicalWeightTpl &w2) {
  return w1.Member() || w2.Member() ? TropicalWeightTpl::NoWeight()          : w2.Value()               ? : w2;
}
TropicalWeightTpl Times();
struct ArcTpl {
  using Weight = TropicalWeightTpl;
};
template <class, class, class> struct ShortestPathOptions {
  ShortestPathOptions(int, int, int32, bool, bool);
};
template <class Arc, class Queue, class ArcFilter>
void SingleShortestPath(ShortestPathOptions<Arc, Queue, ArcFilter>) {
  using Weight = typename Arc::Weight;
  auto f_distance = Weight::Zero();
  while (!0) {
    TropicalWeightTpl __trans_tmp_1 = Times(),
                      plus = Plus(f_distance, __trans_tmp_1);
    if (f_distance != plus)
      f_distance = plus;
  }
}
template <class Arc, class Queue, class ArcFilter>
void ShortestPath(int, int *, int *,
                  ShortestPathOptions<Arc, Queue, ArcFilter> opts) {
  SingleShortestPath(opts);
}
struct ShortestDistanceOptions {
  float delta;
};
struct Trans_NS_script_ShortestPathOptions : ShortestDistanceOptions {
  int32 nshortest;
  bool unique;
};
namespace internal {
template <class, class>
void ShortestPath(int ifst, int *ofst, int *distance,
                  Trans_NS_script_ShortestPathOptions opts) {
  using ArcFilter = int;
  ShortestPathOptions<ArcTpl, int, ArcFilter> sopts(opts.nshortest, opts.unique,
                                                    false, opts.delta, 0);
  ShortestPath(ifst, ofst, distance, sopts);
}
int ShortestPath_ifst;
int ShortestPath_ofst;
Trans_NS_script_ShortestPathOptions ShortestPath_opts;
void ShortestPath() {
  using StateId = int;
  ShortestPath<ArcTpl, StateId>(ShortestPath_ifst, &ShortestPath_ofst,
                                &ShortestPath_distance, ShortestPath_opts);
}
} // namespace internal
