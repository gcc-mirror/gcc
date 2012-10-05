// PR c++/50893
// { dg-do compile { target c++11 } }

class Base
{
  public:
  virtual ~Base() = default;
};

class Derived : public Base
{
  public:
  virtual ~Derived() = default;
};
