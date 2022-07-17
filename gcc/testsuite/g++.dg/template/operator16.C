// PR c++/106179

struct Mat {
  template <typename> Mat();
};
template <typename> struct Mat_;
template <typename _Tp> Mat::Mat() {
  _Tp commaInitializer = commaInitializer.operator Mat_<_Tp>;
}
