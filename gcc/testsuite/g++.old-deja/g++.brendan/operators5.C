// GROUPS passed operators
// Check that operators may be (directly) recursive.

extern "C" int printf (const char *, ...); 

struct base {
	int i;
};

base base_variable;

base operator+ (const base& left, const base& right)
{
	base ret_val;

	ret_val.i = left.i + right.i;
	return ret_val;
}

base operator- (const base& left, int right)
{
	base ret_val;

	ret_val.i = left.i - right;
	return ret_val;
}

// Define the unary ! operator for class base to be the fibonachi
// operator.

base operator! (const base& right)
{
	if (right.i < 2)
		return right;
	else
		return ((!(right-1)) + (!(right-2)));
}

int main ()
{
	base k;

	k.i = 15;
	k = !k;		// fib it!

	if (k.i != 610)
	  { printf ("FAIL\n"); return 1; }
	else
	  printf ("PASS\n");

	return 0;
}
