// Special g++ Options: -w
// GROUPS passed references
// Check that if a reference is initialized to refer to a value
// which is returned from a function call, the actual call to
// the function is only invoked for the original initialization
// of the reference, and not for each subsequent use of the
// reference.
//
// This test fails with G++ 1.35.0- (pre-release).
// Reported 4/4/89 by Kim Smith

extern "C" int printf (const char *, ...); 

struct base {
	mutable int data_member;

	base () {}
	void function_member () const;
};

base base_object;

base base_returning_function ();

int call_count = 0;

int main ()
{
	const base& base_ref = base_returning_function ();

	base_ref.function_member ();
	base_ref.function_member ();
	base_ref.data_member  = 99;

	if (call_count == 1)
	  printf ("PASS\n");
	else
	  printf ("FAIL\n");

	return 0;
}

base base_returning_function ()
{
	base local_base_object;

	call_count++;
	return local_base_object;
}

void base::function_member () const
{
}
