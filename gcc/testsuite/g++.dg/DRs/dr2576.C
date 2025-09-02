// DR 2576 - Undefined behavior with macro-expanded #include directives
// { dg-do preprocess }
// { dg-options "-pedantic-errors" }

#define A <cstddef>
#include A
#define B "cstddef"
#include B
#define C(x) #x
#define D(x) C(x)
#include D(cstddef)
#include "cstddef" ""		// { dg-error "extra tokens at end of '#include' directive" }
#include "cstddef"".h"		// { dg-error "extra tokens at end of '#include' directive" }
#include			// { dg-error "'#include' expects '\"FILENAME\"' or '<FILENAME>'" }
#include E			// { dg-error "'#include' expects '\"FILENAME\"' or '<FILENAME>'" }
#include <cstddef
				// { dg-error "missing terminating '>' character" "" { target *-*-* } .-1 }
#include "cstddef
				// { dg-error "missing terminating \" character" "" { target *-*-* } .-1 }
				// { dg-error "'#include' expects '\"FILENAME\"' or '<FILENAME>'" "" { target *-*-* } .-2 }
#define F cstddef
#include F			// { dg-error "'#include' expects '\"FILENAME\"' or '<FILENAME>'" }
// There is implementation divergence on the following cases (G H through M N)
// between e.g. GCC and clang++.  clang++ fails on trying to include ' cstddef'
// and 'cstd def' and 'stddef .h' and 'cstddef ' headers.
// https://eel.is/c++draft/cpp.include#7.sentence-3 makes the whitespace
// handling implementation defined and the way GCC handles it can allow
// certain use cases which aren't otherwise possible.  One can still
// insert spaces into the <> filenames if it is from the same macro.
#define G <
#define H cstddef>
#include G H
#define I <cstd
#define J def>
#include I J
#define K <stddef
#define L .h>
#include K L
#define M <cstddef
#define N >
#include M N
#define O <cstddef> <cstddef>
#include O			// { dg-error "extra tokens at end of '#include' directive" }
#define P "cstddef" ""
#include P			// { dg-error "extra tokens at end of '#include' directive" }
#define Q "cstddef"".h"
#include Q			// { dg-error "extra tokens at end of '#include' directive" }
