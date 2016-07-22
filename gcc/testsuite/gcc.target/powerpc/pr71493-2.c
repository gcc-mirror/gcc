/* { dg-do compile { target { powerpc*-*-linux* && ilp32 } } } */
/* { dg-options "-O2 -msvr4-struct-return" } */

struct S2 { double d; };

struct S2 foo2 (void)
{
  struct S2 s = { 1.0 };
  return s;
}

/* { dg-final { scan-assembler     "lwz" } } */
/* { dg-final { scan-assembler-not "lfd" } } */
