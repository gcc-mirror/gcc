// g++ 1.36.1 bug 900210_05

// Section 18.3 of the 2.0 Reference Manual says "An implementation
// providing { anachronistic features } should also provide a way for
// the user to ensure that they do not occur in a source file."

// The *only* proper way to "ensure" an absence of anachronstic features
// is for C++ language processors to generate errors (rather than just
// warnings) when such features are used. These errors could perhaps be
// triggered by some set of command line options, or by the absence of
// certain command line options.  (For g++, the -pedantic and -traditional
// options come to mind.)

// The use of errors rather than warnings is important because errors
// usually result in nonzero exit status codes for language processors
// and these nonzero exit stati can be automatically checked during
// normal execution of a Makefile.

// cfront 2.0 provides the +p option which causes errors to be generated for
// all cases of anachronistic usage.

// g++ generates neither errors nor warnings for such usage, even when the
// -ansi and -pedantic options are used.

// Cfront 2.0 passes this test.

// keywords: anachronism, enum types, integral types, implicit type conversions

enum enum0 { enum_value_0 } enum0_object;
int int0_object;

void function ()
{
  enum0_object = int0_object;	/* ERROR - */
}

int main () { return 0; }
