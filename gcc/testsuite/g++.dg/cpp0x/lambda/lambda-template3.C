// PR c++/49554
// { dg-options -std=c++0x }

template<typename T>
  struct base
  {
    struct iterator { };

    iterator begin();
  };

template<typename T>
class flist : public base<T>
{
  typedef base<T> Base;

  typedef typename Base::iterator Base_iterator;
public:

  void
  resize()
  {
    Base_iterator b = Base::begin();

    [b](int i) { return i; };
  }
};

void test01()
{
  flist<int> fl;
  fl.resize();
}
