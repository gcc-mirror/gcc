// These tests cover various crashes found in development
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-continuation-mode=on" }
template<typename T>
struct Base
{
  virtual int f(const int a) pre(i > 5){return a;};
private:
  int i = 2;
};

int Base::f(const int a) // { dg-error "used without template arguments" }
{
  return a;
}

