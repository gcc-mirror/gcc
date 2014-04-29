// Core issue 1351
// { dg-do run }
// { dg-require-effective-target c++11 }

bool fail;
struct A
{
  int i = fail ? throw "noooooooo" : 42;
};

int main()
{
  A a1;
  if (a1.i != 42) return 1;
  fail = true;
  try { A a2; }
  catch (...) { }
}
