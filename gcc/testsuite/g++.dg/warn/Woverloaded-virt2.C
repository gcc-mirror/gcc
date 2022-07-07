// PR c++/20423
// { dg-additional-options -Wall }

class Foo
{
public:
  virtual void f(int);
  virtual void f(short);
};

class Bar : public Foo
{
public:
  virtual void f(short);
};
