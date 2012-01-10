// PR c++/51299
// { dg-options "-Wzero-as-null-pointer-constant" }

class Base
{
  public:
  virtual ~Base();
};

class Derived : public Base
{
};

void foo(Base* b)
{
  Derived* d = dynamic_cast<Derived*>(b);
}

void bar(Base& b)
{
  Derived& d = dynamic_cast<Derived&>(b);
}
