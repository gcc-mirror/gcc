/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("sve+sve2"))) int
foo () {
	return 1;
}

__attribute__ ((target_version ("sve"))) int
foo () { /* { dg-message "previous definition of .foo \\\[\\\[target_version\\(.sve.\\)\\\]\\\]. with type .int\\(void\\)." } */
	return 1;
}

int bar () { return foo (); } /* { dg-error "implicit declaration of function .foo." } */

__attribute__ ((target_version ("sve+sve2"))) int
foo2(); /* { dg-message "previous declaration of .foo2 \\\[\\\[target_version\\(.sve\\\+sve2.\\)\\\]\\\]. with type .int\\(void\\)." } */

int bar2 () { return foo2 (); } /* { dg-error "implicit declaration of function .foo2." } */
