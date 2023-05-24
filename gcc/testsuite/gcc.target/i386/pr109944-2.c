/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef char v16qi __attribute__((vector_size(16)));
v16qi foo (char *b)
{
  return (v16qi){ b[0], b[16], b[32], b[48], b[64], b[80], b[96], b[112],
      b[128], b[144], b[160], b[176], b[192], b[208], b[224], b[240] };
}

/* We do not want to generate a spill/reload
        movq    %rdx, -24(%rsp)
...
        movq    %rax, -16(%rsp)
        movdqa  -24(%rsp), %xmm0
        movups  %xmm0, (%rdi)  */
/* { dg-final { scan-assembler-not "movdq\[^\r\n\]*\[bs\]p\\\), %xmm" } } */
