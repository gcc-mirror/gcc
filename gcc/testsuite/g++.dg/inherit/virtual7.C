// Origin: PR c++/47666
// { dg-do compile }

template <typename T>
struct A
{
  int a;
};

template <typename T>
struct B : public A <T>
{
};

class D : public B <D *>
{
  virtual D & operator= (const D &);
};

class E : virtual public D
{
};

class F : public E
{
  virtual void foo ();
};
