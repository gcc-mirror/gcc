// Test that there is no ICE with outlined contracts, caller side checks and
// Nontrivial types in inlined precondition checks
// { dg-do compile { target c++23 } }
// { dg-options "-fcontracts -fcontracts-client-check=all" }
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
