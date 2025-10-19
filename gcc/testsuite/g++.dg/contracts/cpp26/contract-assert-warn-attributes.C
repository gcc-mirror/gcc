// { dg-options "-std=c++26 -fcontracts " }

int foo (int x)
pre [[unlikley]] ( x > 41 )  // { dg-warning "attributes are ignored on function contract specifiers" }
{
  return x + 1;
}
