// Skip if not native
// Special g++ Options: -O2 -fPIC 
// Build don't link: 
struct T
{
	char*	f1;
	int	f2;
};

void f(T*);
int g();

extern char a1[];

inline int m(int a, int b) {return b < a ? 2 : 1;}

void
h()
{
	T	a[10];
	int	i(0);

	bool	c;
	if (c)
	{		 
		a[i++].f1 = "asf";
		f(a);
		i = 0;
	}

	a[i].f1 = &a1[1];
	a[i].f2 = m(1, g());
	i++;

	a[i].f1 = "zxv";
	a[i].f2 = 0;
}
