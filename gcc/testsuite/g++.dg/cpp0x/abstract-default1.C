// PR c++/83796
// { dg-do compile { target c++11 } }

struct MyAbstractClass
{
  virtual int foo() const = 0;
};

struct TestClass
{
  TestClass(const MyAbstractClass& m = {})  // { dg-error "abstract type" }
  : value_(m.foo()) {}

  int value_;
};

int TestFunction(const MyAbstractClass& m = {})  // { dg-error "abstract type" }
{
  return m.foo();
}

int main()
{
  TestClass testInstance;  // { dg-error "abstract type" }
  TestFunction();  // { dg-error "abstract type" }
}
