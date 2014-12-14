/* { dg-options "-O2 -fno-ipa-icf" } */

void runtime_error (void) __attribute__ ((noreturn));
void compiletime_error (void) __attribute__ ((noreturn, error ("")));

static void
compiletime_check_equals_1 (int *x, int y)
{
  int __p = *x != y;
  if (__builtin_constant_p (__p) && __p)
    compiletime_error ();
  if (__p)
    runtime_error ();
}

static void
compiletime_check_equals_2 (int *x, int y)
{
  int __p = *x != y;
  if (__builtin_constant_p (__p) && __p)
    compiletime_error (); /* { dg-error "call to" } */
  if (__p)
    runtime_error ();
}

void
foo (int *x)
{
  compiletime_check_equals_1 (x, 5);
  compiletime_check_equals_2 (x, 10);
}
