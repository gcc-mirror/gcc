// { dg-do run  }
// check EH with templates
extern "C" void abort ();
extern "C" void exit (int);

template <class T, int n, class U> struct A {
	A() {}
	A(char*) {}
};

void f1()
{
	throw *(new A<double, 47, A<int, 37, short> >);
}

void f2()
{
	throw *(new A<double, 47, A<int, 36, short> >);
}

void f3()
{
	throw A<double, 47, A<int, 37, short> > ("howdy");
}

void f4()
{
	throw A<double, 47, A<int, 36, short> > ("hi michey");
}

int main()
{
	int flag;

	flag = 0;
	try {
		f1();
	}
	catch (A<double, 47, A<int, 36, short> >) {
		abort();
	}
	catch (A<double, 47, A<int, 37, short> >) {
		flag = 1;
	}
	if (!flag)
		abort();

	flag = 0;
	try {
		f2();
	}
	catch (A<double, 47, A<int, 36, short&> >) {
		abort();
	}
	catch (A<double, 47, A<int, 36, short> >) {
		flag = 1;
	}
	if (!flag)
		abort();

	flag = 0;
	try {
		f3();
	}
	catch (A<double, 47, A<int, 36, short> >) {
		abort();
	}
	catch (A<double, 47, A<int, 37, short> >) {
		flag = 1;
	}
	if (!flag)
		abort();

	flag = 0;
	try {
		f4();
	}
	catch (A<double, 47, A<int, 36, short&> >) {
		abort();
	}
	catch (A<double, 47, A<int, 36, short> >) {
		flag = 1;
	}
	if (!flag)
		abort();

	exit(0);
}
