// PR c++/123526
// { dg-do compile }

struct A {
  explicit A (int);
  virtual ~A ();
};

A::A (int)
{
}

A::~A ()
{
}
