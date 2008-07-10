// { dg-do compile }
// { dg-options "-fno-gnu-keywords" }

// Verify that the keyword asm is recognized and that the GNU
// extension typeof is not recognized as a keyword when using
// -fno-gnu-keywords.

int asm;	// { dg-error "before .asm." }
int typeof;	// { dg-bogus "before .typeof." }
