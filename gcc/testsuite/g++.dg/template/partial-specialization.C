// Contributed by Gabriel Dos Reis <gdr@codesourcery.com>
// Origin: philippeb@videotron.ca
// { dg-do compile }

struct B
{
	int i;
};

template <class _T, class _M, _M _T::* _V>
	struct A;

template <class _T, int _T::* _V>
	struct A<_T, int, _V>
	{
	};

int main()
{
	A<B, int, & B::i> a;
}

