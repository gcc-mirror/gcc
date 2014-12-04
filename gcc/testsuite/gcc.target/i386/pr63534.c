/* PR target/pr63534 */
/* { dg-do compile { target { ia32 && fpic } } } */
/* { dg-options "-O2 -fPIC" } */

extern void bar (void);

void
foo (void)
{
  bar ();
  bar ();
}

/* We shouldn't load EBX again.  */
/* { dg-final { scan-assembler-not "movl\[ \t\]%\[^,\]+, %ebx" } } */
