// { dg-do assemble  }
// GROUPS passed access-control
class Base
{
protected:
  virtual void DoSomething() = 0;
};

class Fibber : public Base
{
public:
  void DoBP() {
    DoSomething();
  }
};

class Flat : public virtual Fibber
{
public:
  void DoIt() {
    DoSomething();
  }
};
