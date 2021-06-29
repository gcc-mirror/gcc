// { dg-additional-options "-fmodules-ts -fcontracts -fcontract-role=default:maybe,maybe,ignore" }
module;
#include <cstdio>
module foo;

int fn1(int x)
{
  printf("%s(%d)\n", __FUNCTION__, x);
  return x;
}

int fn_iso(int n) [[ pre: pre_print(n) > 0 ]]
{
  printf("%s(%d)\n", __FUNCTION__, n);
  return n;
}

int pre_print2(int n)
{
  printf("pre_print(%d)\n", n);
  return n;
}

int fn2(int x)
{
  printf("%s(%d)\n", __FUNCTION__, x);
  return x;
}

int fn3(int x)
{
  printf("%s(%d)\n", __FUNCTION__, x);
  return x;
}

