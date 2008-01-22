// PR c++/33959

template <typename T> struct A
{
  struct C
  {
    template <typename U> struct D {};
  };
  template <typename S> static C::D<S> bar (S const &);
};

struct E {};

int
main ()
{
  E e;
  A<E>::bar (e);
}
