// g++ 1.37.1 bug 900519_08

// g++ fails to accept the following legal syntax for an invocation of the
// new operator, in which the type specifier is implicitly "int".

// cfront 2.0 passes this test.

// keywords: syntax, operator new, type specifier, type qualifier

void test ()
{
   new const /* int */ (1);				// gets bogus error
}

int main () { return 0; }
