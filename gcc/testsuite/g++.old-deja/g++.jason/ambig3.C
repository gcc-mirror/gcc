// Testcase for ambiguity between function and variable declaration (8.2).
// Build don't link:

struct A {
  A (int, int);
  int k;
};

void f ()
{
  int i[2], j;
  A a (int (i[1]), j);		// gets bogus error - late parsing XFAIL *-*-*
  A b (int (i[1]), int j);	// function
  a.k = 0;			// gets bogus error - late parsing XFAIL *-*-*
  b (i, j);
}
