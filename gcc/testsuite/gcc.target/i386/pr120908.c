/* { dg-do compile { target { lp64 && fpic } } } */
/* { dg-options "-O2 -fpic -mtls-dialect=gnu -mcmodel=large" } */

extern __thread long bar1;
long *
foo1 (void)
{
  return &bar1;
}

static __thread long bar2;
long *
foo2 (void)
{
  return &bar2;
}
