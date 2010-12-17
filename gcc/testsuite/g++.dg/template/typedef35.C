// Origin c++/45558
// { dg-do compile }

template <typename S, typename T>
struct C
{
  template <typename U>
  struct B
  {
    template <typename W>
    struct E
    {
      explicit E(const W &x) : w(x) {}
      const W &w;
    };
  };
};

struct F;
template <typename X>
struct D
{
  D() {}
};

const D<F> g;
template <typename S, typename T>
struct A
{
  template <typename U>
  struct B : C<S, T>::template B<U>
  {
    typedef typename C<S, T>::template B<U> V;
    static const D<typename V::template E<D<F> > > a;
  };
};

template <typename S, typename T>
template <typename U>
const D<typename C<S, T>::template B<U>::template E<D<F> > >
A<S, T>::B<U>::a = typename C<S, T>::template B<U>::template E<D<F> >(g);
