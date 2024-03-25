/* { dg-do run } */
/* { dg-options "-O2 -std=gnu23" } */

// struct with variably-modified member

struct s { char (*y)[]; } s;

int
foo ()
{
	int n = 10;
	struct s { char (*y)[n]; } t;
	typeof(*(1 ? &s : &t)) u;
	return sizeof(*u.y);
}

int main()
{
	if (10 != foo())
		__builtin_abort();

	return 0;
}

