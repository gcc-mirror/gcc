// { dg-do assemble  }

class base
{
public:
  virtual void method()=0;
};

class der: public base
{
public:
  template<class C>
  void method()
    {
      C foo;
    }
};
