template <template<int> class TT, class T> void f(T)
{
}

template <template<class> class TT, class T> void f(T)
{
}

template <class T> class C {};
template <int> class D {};

int main()
{
	f<C>(1);
	f<D>(1);
}
