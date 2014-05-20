// PR c++/52685

template <typename T>
struct A
{
  template <typename U>
  struct B : public A <B<U> >	// { dg-message "not complete" }
  {
    struct C : public B<U>	// { dg-error "incomplete" }
    {
    };
  };
};
