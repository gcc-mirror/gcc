// PR c++/36107
// { dg-do assemble }
// { dg-require-weak "" }

class Test {
  public:
  Test() __attribute__((weak));
};

void test() {
  Test test;
}
