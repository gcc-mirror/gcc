// g++ 1.37.1 bug 900321_05

// The following code is legal as far as the ANSI C standard, GCC, and
// cfront are concerned, however g++ issues errors for the lines indicated.

// Cfront 2.0 passes this test.

// keywords: operator[], pointers, index

char c;
char *cp;
int i;

void function ()
{
  c = 3["abcdef"];	// gets bogus error
  c = i[cp];		// gets bogus error
}

int main () { return 0; }
