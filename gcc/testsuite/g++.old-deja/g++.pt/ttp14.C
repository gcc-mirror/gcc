// { dg-do run  }
template<class T> class D
{
	T	a;
	public:
		int f();
};

template<class T> int D<T>::f()
{
	return sizeof(T);
}

template<class E,template<class> class DD = D> class C
{
		DD<E> d;
	public:
		int f();
};

template<class E,template<class> class DD> int C<E,DD>::f()
{
	DD<E> d2;
	return d2.f();
}

int main()
{
	C<int> c;
	c.f();
}
