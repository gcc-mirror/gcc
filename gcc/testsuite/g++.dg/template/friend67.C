// PR c++/65619

template <class>
class foo
{
	int i;
	
	template <template <class> class T>
	friend foo<T<int> > func();
};

template <template <class> class T>
foo<T<int> > func()
{
	foo<T<int> > f;
	f.i = 3;
	return f;
}

template <class>
struct test {};

int main()
{
	func<test>();
}
