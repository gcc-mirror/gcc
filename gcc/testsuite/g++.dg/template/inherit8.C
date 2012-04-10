// PR c++/52685

template <typename T>
struct A
{
  template <typename U>
  struct B : public A <B<U> >
  {
    struct C : public B<U>
    {
    };
  };
};
