// PRMS Id: 5070 (testcase 1)
// Build don't link:

struct foo {
  foo(int x = 0) {};
  int IsAlive(void) { return 1; }
};

struct xx {
  int IsOk(int X);
  foo a;
};

int xx::IsOk(int X)
{
  return ((xx::a).IsAlive());	// gets bogus error
}
