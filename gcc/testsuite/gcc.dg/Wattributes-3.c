/* { dg-do compile } */

void __attribute__((dj)) foo() { }	/* { dg-warning "attribute directive ignored" } */

int j __attribute__((unrecognized));	/* { dg-warning "attribute directive ignored" } */
