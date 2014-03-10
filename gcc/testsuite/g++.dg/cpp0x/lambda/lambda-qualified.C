// PR c++/50089
// { dg-do compile { target c++11 } }

struct TestBase
{
  void foo() {}
};

struct Test : TestBase
{
  void foo()
  {
    [this]{
      /*this->*/TestBase::foo(); // ICE without this->
    }();
  }
};
