// PR sanitizer/79783
// { dg-options "-fno-tree-dce" }

struct A
{
  static void foo(const char&) {}
};

struct B
{
  B() { A::foo(char()); }
};

struct C
{
  virtual void bar() const { B b; }
};

C c;
