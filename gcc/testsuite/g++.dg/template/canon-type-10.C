// Contributed by Dodji Seketeli <dodji@redhat.com>
// { dg-do compile }

template<class T>
struct C
{
};

template<class T,
	 template<class TT_T0, template<class TT_T1> class TT_TT> class TT,
	 class U = TT<int, C> >
struct S
{
  void foo(TT<T, C>);
};

template<class T,
	 template<class TT_T0, template<class TT_T1> class TT_TT> class TT,
	 class U>
void
S<T, TT, U>::foo(TT<T, C>)
{
}
