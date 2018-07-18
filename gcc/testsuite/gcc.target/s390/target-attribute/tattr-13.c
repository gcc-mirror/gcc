/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-march=z13" } */

#pragma GCC target ("zvector","zvector,arch=z13","arch=z13,zvector","zvector")
__attribute__ ((target("arch=z13","arch=z13")))
void a(void)
{
}

#pragma GCC target ("arch=z13","arch=z13,arch=z13","arch=z13,arch=z13,arch=z13")
__attribute__ ((target("zvector","arch=z13")))
void
b(void)
{ /* { dg-error "value .zvector. is not supported by attribute .target." } */
}
