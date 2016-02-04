/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-O3 -march=zEC12 -mzarch" } */

__attribute__ ((target("arch=zEC12")))
void htm1(void)
{
  __builtin_tend();
}

__attribute__ ((target("arch=z10")))
void htm0(void)
{
  __builtin_tend(); /* { dg-error "is not supported without -mhtm" } */
}

void htmd(void)
{
  __builtin_tend();
}
