// try throwing 0 cast to a class object

struct A {};

void f()
{
	throw (A*)0;
}

int
main()
{
	int flag;

	flag = 0;
	try {
		f();
	}
	catch (A*) {
		flag = 1;
	}

	if (!flag)
		abort();

	exit (0);
}
