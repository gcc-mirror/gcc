// { dg-do run  }
// check cleanup of partial array objects
extern "C" void abort (void);
extern "C" void exit (int);

int ctor = 0;
int dtor = 0;

int cnt = 1;

struct A {
	int x;
	A();
	A(const A&);
	~A();
};

A::A()
{
	if (cnt == 10)
		throw 57;
	x = cnt++;
	ctor++;
}

A::A(const A&)
{
	if (cnt == 10)
		throw 57;
	x = cnt++;
	ctor++;
}

A::~A()
{
	if (x + 1 != cnt--)
		abort();
	dtor++;
}

void f()
{
	A a[] = {A(), A(), A(), A(), A(), A(), A(), A(), A(), A(), A(), A()};

	throw -1066;
}

int
main()
{
	int flag;

	flag = 0;
	try {
		f();
	}
	catch (int) {
		flag = 1;
	}
	if (!flag)
		abort();
	if (ctor != 9)
		abort();
	if (dtor != 9)
		abort();

	exit(0);
}
