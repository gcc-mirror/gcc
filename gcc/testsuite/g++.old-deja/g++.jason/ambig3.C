// { dg-do assemble  }
// Testcase for ambiguity between function and variable declaration (8.2).

struct A {
  A (int, int);
  int k;
};

void f ()
{
  int i[2], j;
  A a (int (i[1]), j);		// { dg-bogus "" } late parsing
  A b (int (i[1]), int j);	// function
  a.k = 0;			// { dg-bogus "" } late parsing
  b (i, j);
}
