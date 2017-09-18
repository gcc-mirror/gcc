// PR c++/71774
// { dg-do compile { target c++11 } }

class Meow
{
  protected:
    Meow() =default;
    virtual void f() {}
};

class Purr : public Meow
{
  public:
    Purr()
      : Meow{}
    {}
};
