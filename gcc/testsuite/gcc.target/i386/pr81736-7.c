/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */

extern int foo (void);

int
bar (void)
{
  return foo ();
}

/* No need to use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
