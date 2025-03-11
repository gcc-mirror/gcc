// test that attribute syntax is diagnosed when using -fcontracts-nonattr
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr" }

int fun(int n)  [[ pre : n > 0 ]]; // { dg-error "contracts are not available with" }
int fun2(const int n)  [[ post : n > 0 ]]; // { dg-error "contracts are not available with" }

int main()
{
  int x;

  [[assert: x >= 0]]; // { dg-error "ontracts are not available with" }

  return 0;
}
