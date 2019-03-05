// PR c++/82570
// { dg-do compile { target c++17 } }

template< typename Body >
inline void iterate(Body body)
{
	body(10);
}

template< typename Pred >
inline void foo(Pred pred)
{
	iterate([&](int param)
	{
		if (pred(param))
		{
			unsigned char buf[4];
			buf[0] = 0;
			buf[1] = 1;
			buf[2] = 2;
			buf[3] = 3;
		}
	});
}

int main()
{
	foo([](int x) { return x > 0; });
	return 0;
}
