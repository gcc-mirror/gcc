// PR c++/51242
// { dg-do compile { target c++11 } }

enum class MyEnum { A = 1 };

struct MyClass
{
  MyEnum Field1 : 3; // { dg-bogus "warning: 'MyClass::Field1' is too small" }
};
