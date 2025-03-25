// Test that non trivial types work ok with a contract wrapper for virtual
// functions.
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on -fcontracts-nonattr -g3 -fcontracts-on-virtual-functions=P2900R13" }

struct NonTrivial{
  NonTrivial(){};
  NonTrivial(const NonTrivial&){}
  ~NonTrivial(){};
  int x = 0;
};

struct S
{

  virtual void f(const NonTrivial s) post(s.x >1 ) {};

};

int main()
{
  NonTrivial nt;
  S s;
  s.f(nt);
}
