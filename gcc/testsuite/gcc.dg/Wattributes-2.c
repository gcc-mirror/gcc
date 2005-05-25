/* { dg-do compile } */
/* { dg-options -Wno-attributes } */

void __attribute__((dj)) foo() { }	/* { dg-bogus "attribute directive ignored" } */

int j __attribute__((unrecognized));	/* { dg-bogus "attribute directive ignored" } */
