// PR c++/49569
// { dg-options -std=c++0x }

struct A
{
  virtual void f() = 0;
};

struct B: A
{
  int i;
  virtual void f() { }
};

struct C
{
  B b;
  C(): b() { }
  C(const B& b): b(b) { }
};

