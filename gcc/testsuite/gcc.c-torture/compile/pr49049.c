__extension__ typedef unsigned long long int uint64_t;

static int
sub (int a, int b)
{
  return a - b;
}

static uint64_t
add (uint64_t a, uint64_t b)
{
  return a + b;
}

int *ptr;

int
foo (uint64_t arg1, int *arg2)
{
  int j;
  for (; j < 1; j++)
    {
      *arg2 |= sub ( sub (sub (j || 1 ^ 0x1, 1), arg1 < 0x1 <=
						   sub (1, *ptr & j)),
		     (sub ( j != 1 || sub (j && j, 1) >= 0,
		       add (!j > arg1, 0x35DLL))));
    }
}
