#include <vector>

template<class E,template<class> class DD = std::vector> class C
{
		DD<E> d;
	public:
		int f();
};

template<class E,template<class> class DD> int C<E,DD>::f()
{
	DD<E> d2;
	return d2.size();
}

int main()
{
	C<int> c;
	c.f();
}
