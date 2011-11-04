// PR c++/48370
// { dg-do run }

extern "C" void abort();
bool ok;

struct A {
  int i;
  A(int i): i(i) { }
  ~A() { if (!ok) abort(); }
};

struct D { int i; };

struct B: D, A { B(int i): A(i) { } };
struct E: D, virtual A { E(int i): A(i) { } };

struct C
{
  const A& ar1;
  const A& ar2;
  const A& ar3;
};

int main()
{
  C c = { 1, B(2), E(3) };
  ok = true;
}
