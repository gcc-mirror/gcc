// Contributed by Dodji Seketeli <dodji@redhat.com>
// { dg-do compile }

template<class T>
struct C
{
  void bar();
};

template<class T>
void
C<T>::bar()
{
}


template<class U,
	 template<class TT0_T0> class TT0 = C,
	 template<class TT1_T0> class TT1 = TT0>
struct S
{
  C<U> s;

  void foo(TT1<U>);

  void bar()
  {
    foo(s);
  }
};

template<class T,
	 template<class TT0_T0> class TT0,
	 template<class TT1_T0> class TT1>
void
S<T, TT0, TT1>::foo(TT1<T>)
{
  C<T> c;
}
