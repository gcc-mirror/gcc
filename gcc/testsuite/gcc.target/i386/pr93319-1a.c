/* { dg-do assemble { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-mx32 -fPIC -mtls-dialect=gnu2" } */

extern __thread int bar;
static __thread int foo = 30;

int *
test1 (void)
{
  __builtin_printf ("foo: %d\n", foo);
  return &foo;
}

int *
test2 (void)
{
  __builtin_printf ("bar: %d\n", bar);
  return &bar;
}
