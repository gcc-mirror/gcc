// GROUPS passed overloading
// Check that calls to the correct overloaded virtual
// functions are generated even where the type of the formal
// arguments for the overloadings are similar or related.

extern "C" int printf (const char *, ...); 

int proper_method_called = 0;

struct base {
	int member;
	virtual void method (char)
	{
	}
	virtual void method (char *)
	{
	}
};

struct derived : public base {
	int member;
	virtual void method (char)
	{
	}
	virtual void method (char *)
	{
		proper_method_called++;
	}
};

char *message;

int main ()
{
	derived derived_object;

	derived_object.method (message);

	if (proper_method_called != 1)
	  { printf ("FAIL\n"); return 1; }
	else
	  printf ("PASS\n");
}
