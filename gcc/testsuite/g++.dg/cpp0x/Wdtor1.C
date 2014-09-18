// PR c++/62232
// { dg-do compile { target c++11 } }
// { dg-options "-Wnon-virtual-dtor" }

class base
{
protected:
  ~base () {}
  virtual void foo (){};
};
class derive final : public base
{
};
