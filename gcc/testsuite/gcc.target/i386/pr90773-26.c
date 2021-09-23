/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake -mtune-ctrl=avx256_move_by_pieces" } */

struct S
{
  long long s1 __attribute__ ((aligned (8)));
  unsigned s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14;
};

const struct S array[] = {
  { 0, 60, 640, 2112543726, 39682, 48, 16, 33, 10, 96, 2, 0, 0, 4 }
};

void
foo (struct S *x)
{
  x[0] = array[0];
}

/* { dg-final { scan-assembler-times "vmovdqu\[\\t \]%ymm\[0-9\]+, \\(%\[\^,\]+\\)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[\\t \]%ymm\[0-9\]+, 32\\(%\[\^,\]+\\)" 1 } } */
