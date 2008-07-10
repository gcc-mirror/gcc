// { dg-do compile }
// { dg-options "" }

// Verify that asm and the GNU extension typeof are recognized as
// keywords.

int asm;	// { dg-error "before .asm." }
int typeof;	// { dg-error "expected" }
// { dg-error "multiple types" "" { target *-*-* } 8 }
// { dg-error "declaration" "" { target *-*-* } 8 }
