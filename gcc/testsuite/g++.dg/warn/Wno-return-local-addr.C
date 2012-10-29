// { dg-do assemble  }
// { dg-options "-Wno-return-local-addr" }

int& bad1()
{
  int x = 0;
  return x;
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
