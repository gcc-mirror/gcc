// PR c++/78890
// { dg-do compile { target c++11 } }

union Test1 {
  static int kConstant;
};

union Test2 {
  static const int kConstant;
};

const int Test2::kConstant = 10;

int k;

union Test3 {
  static int& kRef;
};

int& Test3::kRef = k;

union Test4 {
  static const int& kRef;
};

const int& Test4::kRef = 10;
