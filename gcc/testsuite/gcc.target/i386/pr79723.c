/* { dg-do compile } */
/* { dg-options "-O3 -msse2 -mno-avx" } */

void memset_pattern_seg_gs(unsigned char * __seg_gs *s, long n)
{
  for (long i = 0; i < n; ++i)
    s[i] = 0;
}

/* { dg-final { scan-assembler "mov\[au\]p.\[ \t\]\[^,\]+, %gs:" } } */
