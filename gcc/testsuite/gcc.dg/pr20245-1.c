/* Bug 20245: the parse error should not result in an ICE.  */
/* { dg-do compile } */
/* { dg-options "" } */

void foo() x; /* { dg-error "-:expected" } */
