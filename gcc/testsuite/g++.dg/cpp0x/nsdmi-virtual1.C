// PR c++/51611
// { dg-options -std=c++0x }
// { dg-do run }

struct A
{
  A(): i(42) { }
  int i;
  int f() { return i; }
};

struct B : virtual A
{
  int j = i + f();
  int k = A::i + A::f();
};

struct C: B { int pad; };

int main()
{
  C c;
  if (c.j != 84 || c.k != 84)
    __builtin_abort();
}

