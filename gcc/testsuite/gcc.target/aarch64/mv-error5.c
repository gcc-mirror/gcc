/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */

__attribute__ ((target_version ("sve+sve2"))) int
foo(); /* { dg-message "previous declaration of .foo \\\[\\\[target_version\\(.sve\\\+sve2.\\)\\\]\\\]. with type .int\\(void\\)." } */

int bar () { return foo (); } /* { dg-error "implicit declaration of function .foo." } */
