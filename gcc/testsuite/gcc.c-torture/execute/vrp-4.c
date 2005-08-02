extern void exit (int);
extern void abort ();

void test(int x, int y)
{
	int c;

	if (x == 1) abort();
	if (y == 1) abort();

	c = x / y;

	if (c != 1) abort();
}

int main()
{
	test(2, 2);
	exit (0);
}
