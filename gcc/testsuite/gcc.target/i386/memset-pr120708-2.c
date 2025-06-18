/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -mprefer-vector-width=256 -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign" } */

void
foo (char *dest)
{
  __builtin_memset (dest, 0, 254);
}

/* { dg-final { scan-assembler "vmovdqu\[ \t]\+%ymm\[0-9\]+, \\(\[^\n\r]*\\)" } } */
