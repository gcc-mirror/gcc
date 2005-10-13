// PR c++/22352

template <class A>
class s
{
  typedef int d;
  template <class s, typename s::d>
  friend class t;
};

s<int> t1;

