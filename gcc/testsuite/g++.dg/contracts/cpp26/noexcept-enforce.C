// Test that there is no crash with default contract violation handler and noexcept_enforce
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=noexcept_enforce " }


void f(int i) pre(i > 0){}

int main()
{
  f(0);
}
