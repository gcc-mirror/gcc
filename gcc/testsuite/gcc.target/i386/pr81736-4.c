/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer" } */

int
foo (int i1, int i2, int i3, int i4, int i5, int i6, int i7)
{
  return i7;
}

/* Need to use a frame pointer.  */
/* { dg-final { scan-assembler "%\[re\]bp" } } */
