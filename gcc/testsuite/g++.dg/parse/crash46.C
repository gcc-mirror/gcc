// PR c++/37532
// { dg-do compile }

void
foo (_Decimal32)	// { dg-error "declared void" "declared" }
{
}
			// { dg-error "was not declared" "not" { target *-*-* } 5 }
void
bar (_Bool)		// { dg-error "declared void" "declared" }
{
}
			// { dg-error "was not declared" "not" { target *-*-* } 10 }
void
baz (_Fract)		// { dg-error "declared void" "declared" }
{
}
			// { dg-error "was not declared" "not" { target *-*-* } 15 }
