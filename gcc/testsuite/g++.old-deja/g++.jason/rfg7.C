// Bug: fixincludes and/or cpp mangle the definition of wchar_t so that this
// doesn't work.
// Build don't link:

#include <stdlib.h>
wchar_t array[] = L"xxx";	// gets bogus error - wchar_t
