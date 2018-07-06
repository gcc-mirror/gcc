// PR c++/81013

struct A
{
  virtual void foo() const;
};

union B : A  // { dg-error "derived union 'B' invalid" }
{
  void foo() const;
};

void bar(const B& b)
{
  b.foo();
}
