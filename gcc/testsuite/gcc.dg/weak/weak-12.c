/* Test for #pragma weak with declaration not at file scope.  */
/* { dg-do compile } */
/* { dg-require-weak "" } */
/* { dg-options "" } */
/* NVPTX's weak is applied to the definition,  not declaration.  */
/* { dg-skip-if "" { nvptx-*-* } } */
/* { dg-skip-if PR119369 { amdgcn-*-* } } */

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
