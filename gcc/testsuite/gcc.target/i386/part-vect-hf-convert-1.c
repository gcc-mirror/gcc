/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times {(?n)vcvttph2w[ \t]} 2 } } */
/* { dg-final { scan-assembler-times {(?n)vcvttph2uw[ \t]} 2 } } */
/* { dg-final { scan-assembler-times {(?n)vcvttph2dq[ \t]} 1 } } */
/* { dg-final { scan-assembler-times {(?n)vcvttph2udq[ \t]} 1 } } */
/* { dg-final { scan-assembler-times {(?n)vcvtw2ph[ \t]} 2 } } */
/* { dg-final { scan-assembler-times {(?n)vcvtuw2ph[ \t]} 2 } } */
/* { dg-final { scan-assembler-times {(?n)vcvtdq2phx[ \t]} 1 } } */
/* { dg-final { scan-assembler-times {(?n)vcvtudq2phx[ \t]} 1 } } */
/* { dg-final { scan-assembler-times {(?n)vcvtph2psx[ \t]} 1 } } */
/* { dg-final { scan-assembler-times {(?n)vcvtps2phxx[ \t]} 1 } } */


void
fix_32 (short* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 2; i++)
    pa[i] =  pb[i];
}

void
fix_64 (short* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 4; i++)
    pa[i] =  pb[i];
}

void
fixuns_32 (unsigned short* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 2; i++)
    pa[i] =  pb[i];
}

void
fixuns_64 (unsigned short* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 4; i++)
    pa[i] =  pb[i];
}

void
float_32 (short* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 2; i++)
    pb[i] =  pa[i];
}

void
float_64 (short* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 4; i++)
    pb[i] =  pa[i];
}

void
floatuns_32 (unsigned short* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 2; i++)
    pb[i] =  pa[i];
}

void
floatuns_64 (unsigned short* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 4; i++)
    pb[i] =  pa[i];
}

void
fix_32si (int* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 2; i++)
    pa[i] =  pb[i];
}

void
fix_32usi (unsigned int* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 2; i++)
    pa[i] =  pb[i];
}

void
float_32si (int* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 2; i++)
    pb[i] =  pa[i];
}

void
float_32usi (unsigned int* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 2; i++)
    pb[i] =  pa[i];
}

void
float_extend (float* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 2; i++)
    pa[i] =  pb[i];
}

void
float_truncate (float* __restrict pa, _Float16* pb)
{
  for (int i = 0; i != 2; i++)
    pb[i] =  pa[i];
}
