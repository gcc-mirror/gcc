// check that an invocation of a virtual function through the base class does not
// check contracts of the derived function, which are definition side contracts
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontracts-nonattr-definition-check=off -fcontracts-on-virtual-functions=P2900R13" }

struct Base
{
  virtual int f(const int a){ return 0;};
};

struct Child : Base
{
  virtual int f(const int a) pre (a > 14) post(r:r >2){ return 1;}
};

int fooBase(Base& b)
{
    return b.f(1);
}

int main(int, char**)
{
  Base b;
  Child c;

  fooBase (c);

  return 0;
}
