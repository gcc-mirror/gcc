/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */

extern int foo (void);

int
bar (void)
{
  return foo ();
}

/* No need to use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
