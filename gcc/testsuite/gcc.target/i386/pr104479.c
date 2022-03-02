/* { dg-do compile } */
/* { dg-options "-march=icelake-server -Ofast -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "\.COND_SHR" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\.COND_FMA" "optimized" } } */

void
cond_shr (unsigned int* __restrict dst,
	  unsigned int* __restrict src,
	  unsigned int* __restrict y,
	  int i_width)
{
  for(int x = 0; x < i_width; x++)
    {
      unsigned int temp = src[x] >> 3;
      dst[x] =  temp > 255 ? temp : y[x];
    }
}


void
cond_fma (float* __restrict dst,
	  float* __restrict src1,
	  float* __restrict src2,
	  float* __restrict src3,
	  unsigned int* __restrict y,
	  int i_width)
{
  for(int x = 0; x < i_width; x++)
    {
      float temp = __builtin_fmaf (src1[x], src2[x], src3[x]);
      dst[x] = temp > 0.0f ? temp : y[x];
    }
}
