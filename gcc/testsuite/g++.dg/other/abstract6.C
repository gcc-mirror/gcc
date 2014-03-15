// PR c++/60532

class A
{
  ~A ();
};
class B : A
{
  virtual void m () = 0;
};
