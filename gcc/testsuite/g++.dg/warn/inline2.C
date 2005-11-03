// PR c++/21627

template<typename T>
struct TPL 
{
  TPL (){}
  ~TPL (){}
  void method () {}
};

template <> TPL<int>::TPL ();
template <> TPL<int>::~TPL ();
template <> void TPL<int>::method ();

void Foo ()
{
  TPL<int> i;
  i.method ();
}

