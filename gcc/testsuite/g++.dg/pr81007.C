// { dg-do compile }
// { dg-options "-O2" }

struct A
{
  A p; // { dg-error "incomplete" }
  virtual void foo();
};

struct B : A {};

void bar(B& b)
{
  b.foo();
}
