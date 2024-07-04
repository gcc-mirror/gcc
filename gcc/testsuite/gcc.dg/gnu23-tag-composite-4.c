/* { dg-do run } */
/* { dg-options "-O2 -std=gnu23" } */

// struct with variable size



int
foo ()
{
	int n = 10;
	struct s { char buf[n]; } s;
	{
		int m = 10;
		struct s { char buf[m]; } t;
		typeof(*(1 ? &s : &t)) u;
		return sizeof(u.buf);
	}
}

int main()
{
	if (10 != foo())
		__builtin_abort();

	return 0;
}

