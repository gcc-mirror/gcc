// Test that there is no ICE with outlined contracts, caller side checks and Nontrivial types
// in precondition checks
// { dg-do compile }
// { dg-options "-std=c++2b -fcontracts -fcontracts-nonattr -fno-contract-checks-outlined -fcontracts-nonattr-client-check=all" }
struct NonTrivial{
  NonTrivial(){};
  NonTrivial(const NonTrivial&){}
  ~NonTrivial(){};
  int x = 0;
};

void f(const NonTrivial s) pre(s.x >0);

void f(const NonTrivial g) {};


int main()
{
  NonTrivial nt;
  f(nt);
}
