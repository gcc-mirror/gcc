int i = 7 / 0; /* { dg-error "not constant" } */
	/* { dg-warning "division by zero" "div by zero" { target *-*-* } 1 } */
