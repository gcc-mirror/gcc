// GROUPS passed destructors
// Check that member and base classes get properly destructed
// when an object of a derived class is destructed via a pointer
// to it which only has a "compile-time" type of "pointer-to-base".
//
// Note that in order for this test to work properly, the destructor
// for the base class should be explicitly declared to be virtual.

extern "C" int printf (const char *, ...); 

int member_destructor_calls = 0;
int middle_destructor_calls = 0;

struct member_type {
	int data_member;
	member_type () {}
	~member_type ();
};

struct base {
	int data_member;
	base () {}
	virtual ~base ();
};

struct middle : public base {
	member_type member1;
	member_type member2;
	middle () {}
	~middle ();		// should be implicitly virtual
};

struct derived : public middle {
	member_type member1;
	member_type member2;
	//~derived () {}
};

int main ()
{
	base* bp = new derived;
	delete bp;
	derived *dp = new derived;
	delete dp;

	if ((member_destructor_calls != 8) || (middle_destructor_calls != 2))
	  { printf ("FAIL\n"); return 1; }
	else
	  printf ("PASS\n");

	return 0;
}

member_type::~member_type ()
{
	member_destructor_calls++;
}

base::~base ()
{
}

middle::~middle ()
{
	middle_destructor_calls++;
}
