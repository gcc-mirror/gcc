// check cleanup of template temporaries
extern "C" void abort ();
extern "C" void exit (int);

int ctor = 0;
int dtor = 0;

template <class T> struct A {
	A() {ctor++;}
	A(int) {ctor++;}
	A(const A&) {ctor++;}
	~A() {dtor++;}
	operator int() {return 0;}
};

template <class T> void ff(T);

template <class T> void ff(T)
{
}

void g(int)
{
}

void f()
{
	int x;

	A<int> a1;
	A<double> a2(37);
	A<long> a3 = A<long>(47);
	A<short> a4 = 97;

	g(A<char*>());

	A<char**>();

	x ? A<char*>() : A<char*>();

	x = 47, A<double*>(), A<int>(39), A<void>(23), -17;

	while (A<short>())
		;
	for (;A<unsigned>(3);)
		;
	if (A<A<double> >())
		;

	ff(A<double>());

	throw 59;
}

int 
main()
{
	int flag = 0;

	try {
		A<unsigned long>();
		f();
	}
	catch (int) {
		A<float>(34);
		flag = 1;
	}

	if (!flag)
		abort();

	if (!ctor || ctor != dtor)
		abort();

	exit(0);
}
