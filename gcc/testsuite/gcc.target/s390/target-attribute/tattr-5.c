/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile { target { lp64 } } } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-O3 -march=z13 -mno-zvector" } */

__attribute__ ((target("zvector")))
void a1(void)
{ /* { dg-error "is not supported by attribute" } */
}
