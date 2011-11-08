// { dg-do compile }
// { dg-options "-fgnu-tm" }

struct S
{
  int i, j, k;
  S();
};

S::S() __transaction_atomic : i(1), j(2), k(3) { }
