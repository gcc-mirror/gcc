/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -mtls-dialect=gnu" } */

static __thread int foo = 30;

int *
test1 (void)
{
  __builtin_printf ("foo: %d\n", foo);
  return &foo;
}
