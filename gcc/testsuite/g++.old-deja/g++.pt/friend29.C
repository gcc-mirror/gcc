// Build don't link:

template <class T> class a {
public:
	friend void foo<>( a<T>& thea );
private:
	T amember;
};

template <class T> void foo( a<T>& thea )
{
	thea.amember = 0;
}

template class a<int>;

