// { dg-do compile }
// { dg-options "-std=c++2b -fcontracts -fcontracts-nonattr " }

struct Base
{
  virtual int f1() pre(true); // { dg-error "Contracts cannot be added to virtual functions" }
  virtual int f2();

};
struct Child : Base
{
  int f2() pre(true); // { dg-error "Contracts cannot be added to virtual functions" }

};


