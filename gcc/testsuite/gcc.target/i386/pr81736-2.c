/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */

int
#ifndef __x86_64__
__attribute__((regparm(3)))
#endif
foo (int i)
{
  return i;
}

/* No need to use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
