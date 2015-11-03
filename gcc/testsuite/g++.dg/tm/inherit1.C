// Testcase from TM TS
// { dg-options "-std=c++14 -fgnu-tm" }

struct B {
  virtual void f() transaction_safe;
};

struct D3 : B
{
  void f() transaction_safe_dynamic override; // { dg-error "" "B::f() is transaction_safe" }
};
