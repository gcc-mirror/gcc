/* Test for #pragma weak with declaration not at file scope.  */
/* { dg-do compile } */
/* { dg-require-weak "" } */
/* { dg-options "" } */

/* { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?foo" } } */

#pragma weak foo

int
main (void)
{
  extern int foo (void);
  if (&foo)
    return foo ();
  return 0;
}
