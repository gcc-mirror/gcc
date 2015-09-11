// PR 32519
// { dg-do compile }

struct B
{
protected:
  template <class T> void f (); // { dg-message "protected" }
};

struct D : public B
{
  void g (B* b)
  {
    b->f<int> (); // { dg-error "context" }
  }
};
