/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */

extern int i;

int
foo (void)
{
  return i;
}

/* No need to use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
