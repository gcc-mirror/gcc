// PR c++/37532
// { dg-do compile }

void
foo (_Decimal32)	// { dg-error "declared void" }
{
}
			// { dg-error "was not declared" "" { target *-*-* } 5 }
void
bar (_Bool)		// { dg-error "declared void" }
{
}
			// { dg-error "was not declared" "" { target *-*-* } 10 }
void
baz (_Fract)		// { dg-error "declared void" }
{
}
			// { dg-error "was not declared" "" { target *-*-* } 15 }
