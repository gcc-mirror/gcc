// { dg-do assemble  }
// Bug: fixincludes and/or cpp mangle the definition of wchar_t so that this
// doesn't work.

#include <stdlib.h>
wchar_t array[] = L"xxx";	// { dg-bogus "" } wchar_t
