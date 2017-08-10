/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */

void
foo (void)
{
  asm ("# " : : : "ebx");
}

/* Need to use a frame pointer.  */
/* { dg-final { scan-assembler "%\[re\]bp" } } */
