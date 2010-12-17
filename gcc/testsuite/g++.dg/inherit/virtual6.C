// PR c++/45473

struct A
{
  virtual void B ();
};

struct B : A
{
  B ();
};

struct C : B
{
};
