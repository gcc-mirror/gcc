/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -mprefer-vector-width=128 -mmemcpy-strategy=vector_loop:2048:noalign,libcall:-1:noalign" } */

#define SIZE (16 + 1) * 16

char dest[SIZE];
char src[SIZE];

void
foo (void)
{
  __builtin_memcpy (dest, src, SIZE);
}

/* { dg-final { scan-assembler-times "vmovdqa\[ \t]\+\[^\n\r]*%xmm\[0-9\]\+" 10 } } */
