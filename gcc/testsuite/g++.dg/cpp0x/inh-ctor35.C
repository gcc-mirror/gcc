// PR c++/92590
// { dg-do compile { target c++11 } }

class Base {
  protected:
    Base();
};

class Derived : public Base {
  public:
    using Base::Base;
};

Derived d;
