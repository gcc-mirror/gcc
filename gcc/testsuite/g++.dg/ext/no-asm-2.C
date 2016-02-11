// { dg-do compile }
// { dg-options "-fno-asm" }

// Verify that the keyword asm and the GNU extension typeof are not
// recognized as keywords when using -fno-asm.  Having -fno-asm affect
// a standard C++ keyword seems strange, but that is existing
// behavior.  If that behavior changes, this test should change.

int asm;	// { dg-bogus "before .asm." }
int typeof;	// { dg-bogus "before .typeof." }
