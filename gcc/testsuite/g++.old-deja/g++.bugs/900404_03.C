// g++ 1.37.1 bug 900404_03

// g++ fails to be able to properly flag errors for even simple cases of
// ambiguous overload resolution (such as the one shown below).

// Cfront 2.0 passes this test.

// keywords: overloading, ambiguity, resolution

void function0 (int i, char c)
{				// ERROR - 
  i = c;
}

void function0 (char c, int i)
{				// ERROR - 
  i = c;
}

char c;

void test ()
{
  function0 (c,c);		// ERROR - missed
}

int main () { return 0; }
