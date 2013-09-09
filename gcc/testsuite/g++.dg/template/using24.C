// PR c++/21682

template <class T>
struct t
{
  typedef typename T::type type;
};
template<> class t<int>{};

template <class T> struct t1{ };
template<> struct t1<int>
{
  typedef int type;
};

namespace name1
{
  template <class S> typename t<S>::type begin(S const& s);
  namespace name2
  {
    template <class S> typename t1<S>::type begin(S const& s);
  }
  using name2::begin;
}

/* Test calling the function. */
int f(int a) { return name1::begin(a); }

struct aa { typedef double type; };
double g(aa t) { return name1::begin(t); }
