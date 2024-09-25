/* { dg-do compile } */
/* { dg-options "-Wno-implicit-int" } */

a();	/* { dg-warning "no type or storage" } */
a;	/* { dg-error "redeclared" } */
	/* { dg-warning "no type or storage" "" { target *-*-* } .-1 } */
a();	/* { dg-warning "no type or storage" } */
