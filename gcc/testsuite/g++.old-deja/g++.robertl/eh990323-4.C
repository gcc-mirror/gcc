// { dg-do run  }
// check MI and VBC offsets on throw
extern "C" void abort ();
extern "C" void exit (int);

struct A {
	int x[23];
};

struct B : virtual public A {
	int y[33];
};

struct C : virtual public A, public B {
	int z[43];
};

struct D {
	int xx[53];
};

struct E : public D, public A {
	int yy[63];
};

C c;

E e;

void f1()
{
	throw (C*)0;
}

void f2()
{
	throw &c;
}

void f3()
{
	throw (E*)0;
}

void f4()
{
	throw &e;
}

int
main()
{
	int flag;

	flag = 0;
	try {
		f1();
	}
	catch (void* p) {
		if (p)
			abort();
		flag = 1;
	}
	if (!flag)
		abort();

	flag = 0;
	try {
		f2();
	}
	catch (void* p) {
		if (!p || (void*)p != (void*)&c)
			abort();
		flag = 1;
	}
	if (!flag)
		abort();

	flag = 0;
	try {
		f3();
	}
	catch (void* p) {
		if (p)
			abort();
		flag = 1;
	}
	if (!flag)
		abort();

	flag = 0;
	try {
		f4();
	}
	catch (void* p) {
		if (!p || (void*)p != (void*)&e)
			abort();
		flag = 1;
	}
	if (!flag)
		abort();

	exit(0);
}
