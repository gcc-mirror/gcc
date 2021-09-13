/* Test __attribute__ ((unavailable)) */
/* { dg-do compile } */
/* { dg-options "" } */

struct Foo { int i; } __attribute__ ((unavailable));
void foo() { Foo f; }		// { dg-error "unavailable" }
