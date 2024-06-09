/* { dg-do compile { target i?86-*-* x86_64-*-* rs6000-*-* s390x-*-* } } */
/* { dg-options "-O2 -fstack-protector-all" } */

void
bar (void)
{
  throw 1;
}

/* { dg-final { scan-assembler-times "stack_chk_fail" 1 } } */

/* When compiling for PI[EC], we issue a .hidden stack_chk_fail_local,
   that causes the above to fail the expected match count.  */
/* { dg-additional-options "-fno-PIC" } */
