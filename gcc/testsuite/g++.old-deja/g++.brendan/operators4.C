// { dg-do assemble  }
// GROUPS passed operators
// Check that the & operator, when applied to a global function
// or member function returns a proper value as long as the context
// in which the result of & is used requires a pointer to a specific
// unambigous (function-pointer) type.
//
// This test fails (in test5()) when compiled with g++ 1.34.1.

extern "C" int printf (const char *, ...); 

int function (char c);
int function (float f);

class base {
	int filler;
public:
	int method (char);
	int method (float);
};

void* vp;

typedef int (*ptr_to_func_of_char)(char);
typedef int (*ptr_to_func_of_float)(float);
typedef int (base::*ptr_to_method_of_char)(char);
typedef int (base::*ptr_to_method_of_float)(float);

int test2 (void*);
int test3 (void*);
int test4 (void*);
int test5 (void*);

base* base_ptr;

int fail ()
{
  printf ("FAIL\n");
  return 1;
}

int main ()
{
	base_ptr = new base;

	ptr_to_func_of_char p0 = &function;
	vp = (void*) p0;
	if (test2 (vp))
		return fail ();
	ptr_to_func_of_float p1 = &function;
	vp = (void*) p1;
	if (test3 (vp))
		return fail ();
	ptr_to_method_of_char p2 = &base::method;
	vp = (void*) p2; // { dg-error "" } 
	if (test4 (vp))
		return fail ();
	ptr_to_method_of_float p3 = &base::method;
	vp = (void*) p3; // { dg-error "" } 
	if (test5 (vp))
		return fail ();

	printf ("PASS\n");
	return 0;
}

int test2 (void* vp)
{
	char ch = 'x';

	return (((ptr_to_func_of_char)vp)(ch) !=  9901);
}

int test3 (void* vp)
{
	float flt = 9.9;

	return (((ptr_to_func_of_float)vp)(flt) !=  9902);
}

int test4 (void* vp)
{
	char ch = 'x';
	ptr_to_method_of_char p = (ptr_to_method_of_char) vp; // { dg-error "" } bad type conversion

	return ((base_ptr->*p)(ch) !=  9904);
}

int test5 (void* vp)
{
	float flt = 9.9;
	ptr_to_method_of_float p = (ptr_to_method_of_float) vp; // { dg-error "" } bad type conversion

	if ((base_ptr->*p)(flt) !=  9905) {
		return 1;
	} else
		return 0;
}

int function (char c)
{
	c = c;
	return 9901;
}

int function (float f)
{
	f = f;
	return 9902;
}

int base::method (char c)
{
	c = c;
	return 9904;
}

int base::method (float f)
{
	f = f;
	return 9905;
}
