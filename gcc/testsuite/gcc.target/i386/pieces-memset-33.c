/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx2 -mavx -mtune-ctrl=avx256_move_by_pieces" } */

extern char *dst;

void
foo (void)
{
  __builtin_memset (dst, -1, 33);
}

/* { dg-final { scan-assembler-not "vpcmpeqd\[ \\t\]+\[^\n\]*%ymm" } } */
/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\n\]*%ymm" 1 } } */
/* No need to dynamically realign the stack here.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*%\[re\]sp" } } */
/* Nor use a frame pointer.  */
/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
