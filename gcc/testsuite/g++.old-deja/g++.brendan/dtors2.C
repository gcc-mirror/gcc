// { dg-do run  }
// GROUPS passed destructors
// Check that virtual destructors work correctly.  Specifically,
// check that when you destruct an object of a derived class for
// which the base class had an explicitly declared virtual destructor
// no infinite recursion occurs.
//
// Bug description:
//    The generated g++ code apparently calls the base class destructor via
//    the virtual table, rather than directly. This, of course, results in the
//    infinite recursion.

extern "C" int printf (const char *, ...); 

int errors = 0;

struct base {
	int member;
	base();
	virtual ~base();
};
  
base::base()
{
}

base::~base()
{
}

struct derived : public base
{
	int member;
	derived();
	~derived();
};
  
derived::derived() : base()
{
}

int derived_destructor_calls = 0;

extern void exit (int);

derived::~derived()
{
	if (++derived_destructor_calls > 2)
		errors++;
}

void test ();

int main ()
{
	test ();

	if (errors)
	  { printf ("FAIL\n"); return 1; }
	else
	  printf ("PASS\n");

	return 0;
}

base* bp;

void test()
{
	derived a;

	a.member = 99;
	bp = new derived;
	delete bp;
}
