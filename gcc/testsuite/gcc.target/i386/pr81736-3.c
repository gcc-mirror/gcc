/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer" } */

void
foo (void)
{
  asm ("# " : : : "ebx");
}

/* Need to use a frame pointer.  */
/* { dg-final { scan-assembler "%\[re\]bp" } } */
