/* { dg-do compile { target { ! nvptx*-*-* } } } */
/* { dg-options "-O2 -fpatchable-function-entry=3,1" } */
/* { dg-final { scan-assembler-times "nop" 2 { target { ! alpha*-*-* } } } } */
/* { dg-final { scan-assembler-times "bis" 2 { target alpha*-*-* } } } */

extern int a;

/* Respect overriding attributes in the declaration.  */
int f3 (void) __attribute__((patchable_function_entry(2)));

/* F3 should now get 2 NOPs.  */
int
__attribute__((noinline))
f3 (void)
{
  return 5*a;
}
