// { dg-options "-std=c++98 -pedantic-errors" }

enum E { 
  a = 24.2, // { dg-error "constant" }
  b = (int)3.7, 
  c = int(4.2),
  d = (int)(4.2 + 3.7), // { dg-error "constant" }
  e = int(4.2 - 3.7), // { dg-error "constant" }
  f = (int)17.25
};

struct S {
  static const int i = (int)4.2;
  int j[(int)4.2];
  static const int k = static_cast<short>(3.7);
};
