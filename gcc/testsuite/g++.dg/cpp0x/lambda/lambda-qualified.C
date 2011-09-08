// PR c++/50089
// { dg-options -std=c++0x }

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
