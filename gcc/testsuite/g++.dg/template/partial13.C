// PR c++/45012

template <bool B, class T=void> struct enable_if;

template <class T>
struct enable_if<true,T>
{
  typedef T type;
};

enum { RUNTIME = 0 };
// it compiles with the previous line commented out and the next commented in
// static const int RUNTIME=0;

template <class T, class U, class EN=void> struct foo;

template <template<int> class V, int M>
struct foo<V<M>,V<M>, typename enable_if<M==RUNTIME||M==2>::type> {};

template <template<int> class V1, template<int> class V2, int M>
struct foo<V1<M>,V2<M>, typename enable_if<M==RUNTIME||M==2>::type> {};

template <int M> struct bar {};

foo<bar<2>,bar<2> > x;
