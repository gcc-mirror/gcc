// Build don't link: 
// GROUPS passed old-abort
template <class T> class bug {

public:
    void		Foo(const int = 0);
    void		NotRedeclared(const int);

private:
	T		TheItem;
};

template <class T> void bug<T>::NotRedeclared(const int)
{
}

template <class T> void bug<T>::Foo(const int)
{
}

int
main()
{
	bug<char>	InstantiatedBug;

	return 0;
}
