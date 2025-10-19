// N5008:
// dcl.contract.func/p6
// A virtual function (11.7.3),
// TODO : a deleted function (9.6.3), or a function defaulted on its first declaration (9.6.2)
// shall not have a function-contract-specifier-seq.
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts" }

struct Base
{
  virtual int f1() pre(true); // { dg-error "contracts cannot be added to virtual functions" }
  virtual int f2();

};
struct Child : Base
{
  int f2() pre(true); // { dg-error "contracts cannot be added to virtual functions" }

};


