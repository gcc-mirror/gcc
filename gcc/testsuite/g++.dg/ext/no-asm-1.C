// { dg-do compile }
// { dg-options "" }

// Verify that asm and the GNU extension typeof are recognized as
// keywords.

int asm;	// { dg-error "before .asm." }
int typeof;	// { dg-error "expected" "expected" }
// { dg-error "multiple types" "multiple" { target *-*-* } .-1 }
// { dg-error "declaration" "declaration" { target *-*-* } .-2 }
