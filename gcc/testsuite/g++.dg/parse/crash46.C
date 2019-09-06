// PR c++/37532
// { dg-do compile }

void
foo (_Decimal32)	// { dg-error "1:variable or field .foo. declared void" "declared" }
{
}
			// { dg-error "was not declared" "not" { target *-*-* } 5 }
void
bar (_Bool)		// { dg-error "1:variable or field .bar. declared void" "declared" }
{
}
			// { dg-error "was not declared" "not" { target *-*-* } 10 }
void
baz (_Fract)		// { dg-error "1:variable or field .baz. declared void" "declared" }
{
}
			// { dg-error "was not declared" "not" { target *-*-* } 15 }
