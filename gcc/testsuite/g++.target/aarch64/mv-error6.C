/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("sve+sve2"))) int
foo () {
	return 1;
}

__attribute__ ((target_version ("sve"))) int
foo () {
	return 1;
}

int bar () { return foo (); } /* { dg-error "no matching function for call to" } */

__attribute__ ((target_version ("sve+sve2"))) int
foo2();

int bar2 () { return foo2 (); } /* { dg-error "no default version in scope" } */
