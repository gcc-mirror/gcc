// Build don't link:

template <int i> class C {};
template <template <long> class TT> class D {};

int main()
{
	D<C> d;		// ERROR - args not match
}
