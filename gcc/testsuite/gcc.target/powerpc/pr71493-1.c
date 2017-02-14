/* { dg-do compile { target { powerpc*-*-linux* && ilp32 } } } */
/* { dg-options "-O2 -msvr4-struct-return" } */

struct S1 { float f; };

struct S1 foo1 (void)
{
  struct S1 s = { 1.0f };
  return s;
}

/* { dg-final { scan-assembler     "lwz" } } */
/* { dg-final { scan-assembler-not "lfs" } } */
