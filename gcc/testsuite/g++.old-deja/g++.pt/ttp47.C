template <template<class,class> class TT, class T> void f(T)
{
}

template <template<class> class TT, class T> void f(T)
{
}

template <class T> class C {};
template <class T,class U> class D {};

int main()
{
	f<C>(1);
	f<D>(1);
}
