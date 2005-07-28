int a = 0x101;
int b = 0x100;

int
test (void)
{
  return (((unsigned char) (unsigned long long) ((a ? a : 1) & (a * b)))
	  ? 0 : 1);
}

int
main (void)
{
  return 1 - test ();
}
