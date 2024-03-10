/* { dg-do compile } */
/* { dg-options "-O2" } */

void foo (char * __restrict a, char *b)
{
  a[0] = b[0];
  a[1] = b[16];
  a[2] = b[32];
  a[3] = b[48];
  a[4] = b[64];
  a[5] = b[80];
  a[6] = b[96];
  a[7] = b[112];
  a[8] = b[128];
  a[9] = b[144];
  a[10] = b[160];
  a[11] = b[176];
  a[12] = b[192];
  a[13] = b[208];
  a[14] = b[224];
  a[15] = b[240];
}

/* We do not want to generate a spill/reload for when the store is vectorized.
        movq    %rdx, -24(%rsp)
...
        movq    %rax, -16(%rsp)
        movdqa  -24(%rsp), %xmm0
        movups  %xmm0, (%rdi)  */
/* { dg-final { scan-assembler-not "movdq\[^\r\n\]*\[bs\]p\\\), %xmm" } } */
