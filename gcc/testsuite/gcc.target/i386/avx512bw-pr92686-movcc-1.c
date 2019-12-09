/* PR target/92686 */
/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512bw -mno-avx512dq -mno-avx512vl -mno-xop -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler-times "vpcmp\[bwdq\]\[\t ]" 8 } } */
/* { dg-final { scan-assembler-times "vpcmpu\[bwdq\]\[\t ]" 8 } } */
/* { dg-final { scan-assembler-times "vmovdq\[au\]8\[^\{\n\]*%zmm0+\[^\n\]*\{%k\[1-7\]\}" 4 } } */
/* { dg-final { scan-assembler-times "vmovdq\[au\]16\[^\{\n\]*%zmm0+\[^\n\]*\{%k\[1-7\]\}" 4 } } */
/* { dg-final { scan-assembler-times "vmovdq\[au\]32\[^\{\n\]*%zmm0+\[^\n\]*\{%k\[1-7\]\}" 4 } } */
/* { dg-final { scan-assembler-times "vmovdq\[au\]64\[^\{\n\]*%zmm0+\[^\n\]*\{%k\[1-7\]\}" 4 } } */

__attribute__((noipa)) void
f1 (char *__restrict dst, char *__restrict src1, char *__restrict src2)
{
  for (int i = 0; i != 64; i++)
    dst[i] = src1[i] >= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f2 (unsigned char *__restrict dst, unsigned char *__restrict src1,
    unsigned char *__restrict src2)
{
  for (int i = 0; i != 64; i++)
    dst[i] = src1[i] >= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f3 (char *__restrict dst, char *__restrict src1, char *__restrict src2)
{
  for (int i = 0; i != 64; i++)
    dst[i] = src1[i] <= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f4 (unsigned char *__restrict dst, unsigned char *__restrict src1,
    unsigned char *__restrict src2)
{
  for (int i = 0; i != 64; i++)
    dst[i] = src1[i] <= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f5 (short *__restrict dst, short *__restrict src1, short *__restrict src2)
{
  for (int i = 0; i != 32; i++)
    dst[i] = src1[i] >= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f6 (unsigned short *__restrict dst, unsigned short *__restrict src1,
    unsigned short *__restrict src2)
{
  for (int i = 0; i != 32; i++)
    dst[i] = src1[i] >= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f7 (short *__restrict dst, short *__restrict src1, short *__restrict src2)
{
  for (int i = 0; i != 32; i++)
    dst[i] = src1[i] <= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f8 (unsigned short *__restrict dst, unsigned short *__restrict src1,
    unsigned short *__restrict src2)
{
  for (int i = 0; i != 32; i++)
    dst[i] = src1[i] <= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f9 (int *__restrict dst, int *__restrict src1, int *__restrict src2)
{
  for (int i = 0; i != 16; i++)
    dst[i] = src1[i] >= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f10 (unsigned int *__restrict dst, unsigned int *__restrict src1,
     unsigned int *__restrict src2)
{
  for (int i = 0; i != 16; i++)
    dst[i] = src1[i] >= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f11 (int *__restrict dst, int *__restrict src1, int *__restrict src2)
{
  for (int i = 0; i != 16; i++)
    dst[i] = src1[i] <= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f12 (unsigned int *__restrict dst, unsigned int *__restrict src1,
     unsigned int *__restrict src2)
{
  for (int i = 0; i != 16; i++)
    dst[i] = src1[i] <= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f13 (long long int *__restrict dst, long long int *__restrict src1,
     long long int *__restrict src2)
{
  for (int i = 0; i != 8; i++)
    dst[i] = src1[i] >= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f14 (unsigned long long int *__restrict dst,
     unsigned long long int *__restrict src1,
     unsigned long long int *__restrict src2)
{
  for (int i = 0; i != 8; i++)
    dst[i] = src1[i] >= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f15 (long long int *__restrict dst, long long int *__restrict src1,
     long long int *__restrict src2)
{
  for (int i = 0; i != 8; i++)
    dst[i] = src1[i] <= src2[i] ? src1[i] : dst[i];
}

__attribute__((noipa)) void
f16 (unsigned long long int *__restrict dst,
     unsigned long long int *__restrict src1,
     unsigned long long int *__restrict src2)
{
  for (int i = 0; i != 8; i++)
    dst[i] = src1[i] <= src2[i] ? src1[i] : dst[i];
}
