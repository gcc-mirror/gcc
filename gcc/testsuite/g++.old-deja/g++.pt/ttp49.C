// { dg-do assemble  }

template <int i> class C {};
template <template <long> class TT> class D {};

int main()
{
	D<C> d;		// { dg-error "" } args not match
}
