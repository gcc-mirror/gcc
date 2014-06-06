// PR c++/60184
// { dg-do compile { target c++11 } }

union Test1 {
  static constexpr int kConstant = 10;
};

union Test2 {
  static constexpr const int& kConstant = Test1::kConstant;
};
