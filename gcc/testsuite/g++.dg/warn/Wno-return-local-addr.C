// { dg-do assemble  }
// { dg-options "-Wno-return-local-addr" }

int& bad1()
{
  int x = 0;
  return x; // { dg-error "cannot bind non-const lvalue reference" "" { target c++23 } }
}

int* bad2()
{
  int x = 0;
  return &x;
}

int f();

const int& bad3()
{
  return f();
}

const int& bad4()
{
  return int();
}
