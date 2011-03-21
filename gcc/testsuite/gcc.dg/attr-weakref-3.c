/* { dg-do compile } */
/* { dg-require-weak "" } */
static int i __attribute__ ((weakref)); /* { dg-warning "attribute should be accompanied" } */
