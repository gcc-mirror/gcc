// check that we do not a section type conflict with virtual bases or a duplicate symbol
// { dg-do run }
// { dg-options "-fcontracts -std=c++23 -fcontracts-nonattr -fcontracts-on-virtual-functions=P2900R13" }

int x = 9;
struct Base
{
  virtual void f(){};
};


struct Child0 : virtual Base
{
  virtual void f() pre(x>2) {};
};



int main(int, char**)
{
  Base b0;
  Child0 c0;
  c0.f();
}
