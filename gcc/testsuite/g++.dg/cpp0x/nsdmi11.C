// PR c++/66320
// { dg-do compile { target c++11 } }

class A
{
  virtual int m_fn1 ();
};
class B
{
public:
  B (int);
};
class D : B
{
  struct C
  {
    A a;
    A b = a;
  };
  D (int *);
  C _channels;
};
D::D (int *) : B (0) {}
