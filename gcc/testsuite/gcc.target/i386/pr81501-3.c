/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fplt -mtls-dialect=gnu2" } */

static __thread int local1;
int *
get_local1 (void)
{
  return &local1;
}
