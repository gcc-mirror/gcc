// PR c++/60184
// { dg-do compile { target c++11 } }

union Test1 {
  static int kConstant;
};

union Test2 {
  static const int kConstant;
};

const int Test2::kConstant = 10;

union Test3 {
  int& kConstant;
};

union Test4 {
  const int& kConstant = 10;
};
