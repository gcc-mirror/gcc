// Build don't link:

template<class K>
struct A {
	int foo(const K&);
	int bar(const K&);
};

template<class K>
int
A<K>::bar(const K& k)
{
	return(foo(k));
}

template<>
int
A<const char*>::foo(const char*const& k)
{
        return((int)k);
}
