// g++ 1.37.1 bug 900324_06

// g++ is unable to use context information (i.e. the required type of the
// expression) to disambiguate a possibly overloaded function name when that
// name is used as either the second or the third operand of a ?: operator.

// It is also unable to use the fact that the given name is not in fact
// overloaded (and has only one possible interpretation).

// This results in improper errors being generated.

// keywords: overloading, function pointers, disambiguation, operator?:

int i;
void (*p)();

void function_0 ()
{
}

void function_1 ()
{
  p = i ? function_0 : 0;		// gets bogus error
  p = i ? 0 : function_0;		// gets bogus error
  p = i ? function_1 : function_0;	// gets bogus error
}

int main () { return 0; }
