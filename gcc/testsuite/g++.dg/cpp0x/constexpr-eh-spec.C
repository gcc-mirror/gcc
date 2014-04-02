// { dg-do compile { target c++11 } }
template<class T> class my_limits {
public:
  static constexpr T min() throw() { return T(); }
  static constexpr T max() noexcept { return T(); }
};

constexpr double var_min = my_limits<double>::min(); // #1  OK
constexpr double var_max = my_limits<double>::max(); // #2 Error
