/* { dg-do assemble { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-mx32 -fPIC -mtls-dialect=gnu2" } */

#include <stdio.h>

extern __thread int bar;
static __thread int foo = 30;

int *
test1 (void)
{
  printf ("foo: %d\n", foo);
  return &foo;
}

int *
test2 (void)
{
  printf ("bar: %d\n", bar);
  return &bar;
}
