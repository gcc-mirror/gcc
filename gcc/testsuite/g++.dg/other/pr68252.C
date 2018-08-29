// PR c++/68252

struct Test {
  static const int foo = (1 << sizeof (int)) * -3;
};
