// Special g++ Options: -Wno-deprecated -fthis-is-variable
// GROUPS passed code-generation
// Check that the "this" pointer is zero when a method is
// called for an object pointed to by a null pointer.

// Normally, the "__builtin_new" operation which actually
// allocates objects in heap space is *not* called at the
// actual point of the "new" keyword.  Rather, a check is
// made within each constructor and if the "this" pointer
// value passed in is zero, then the actual allocation of
// memory (via __builtin_new) is done at that point (i.e.
// at the very beginning of the constructor).

// A special trick allows one to subvert this mechanism.
// Specifically, if a given constructor contains a statement
// like: "this = this", then no attempt will be made to
// implicitly call __builtin_new within that constructor.

extern "C" void printf (char *, ...); 

struct base {
	int member;

	base ();
	void member_function ();
};

base *base_pointer_1 = 0;
base *base_pointer_2 = 0;

int errors = 0;

int main ()
{
	//base_pointer_2 = new base();
	base_pointer_1->member_function ();

	if (errors)
	  printf ("FAIL\n");
	else
	  printf ("PASS\n");

	return 0;
}

base::base ()
{
	this = this;
	if ((int) this != 0)
		errors++;
}

void base::member_function ()
{
	if ((int) this != 0)
		errors++;
}
