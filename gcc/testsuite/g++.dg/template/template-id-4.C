// PR c++/48157

struct AType
{
  template<class AA>
  void SomeFuncTemplate()
  { }
};

template < class T >
struct TTest2
{
  template<T> struct helper;

  template<class U>
  static void check(helper<&U::template SomeFuncTemplate<int> > *);
};

int main()
{
  TTest2< void (AType::*)() >::check<AType>(0);
}
