// { dg-do assemble  }

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
        return((__SIZE_TYPE__)k);
}
