// GROUPS passed operator-new
// Check that if there is a user defined class-specific operator
// new for a given class, that operator is invoked when a new
// object of the class is requested, regardless of whether or not
// there is also a constructor for the given class, and regardless
// of whether or not the constructor for the given class is defined
// before or after the new operator is even declared.

extern "C" void printf (char *, ...); 

typedef __SIZE_TYPE__ size_t;

struct base {
	int i;

	base ()
	{
		i = 7;
	}

	void * operator new (size_t size);
	void operator delete (void*);
};

class derived : public base {
	int j;
};

int new_call_count = 0;
int expected_size = 0;
int errors = 0;

int main ()
{
	base*		base_ptr;
	derived*	derived_ptr;

	expected_size = 4;
	base_ptr = new base;
	expected_size = 8;
	derived_ptr = new derived ();

	if ((new_call_count != 2) || (errors != 0))
	  printf ("FAIL\n");
	else
	  printf ("PASS\n");

	return 0;
}

char allocation_space[1000];
char* allocation_ptr = allocation_space;

void base::operator delete (void* p)
{
}

void *base::operator new (size_t size)
{
	char* return_value = allocation_ptr;

	new_call_count++;
	if (size != expected_size)
		errors++;
	allocation_ptr = allocation_ptr + size;
	return (void*) return_value;
}
