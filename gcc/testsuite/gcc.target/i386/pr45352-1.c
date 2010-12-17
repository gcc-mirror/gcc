/* { dg-do compile } */
/* { dg-options "-mtune=amdfam10 -O3 -fpeel-loops -fselective-scheduling2 -fsel-sched-pipelining -fPIC" } */

static int FIR_Tab_16[16][16];

void
V_Pass_Avrg_16_C_ref (int *Dst, int *Src, int W, int BpS, int Rnd)
{
  while (W-- > 0)
    {
      int i, k;
      int Sums[16] = { };
      for (i = 0; i < 16; ++i)
	for (k = 0; k < 16; ++k)
	  Sums[k] += FIR_Tab_16[i][k] * Src[i];
      for (i = 0; i < 16; ++i)
	Dst[i] = Sums[i] + Src[i];
    }
}
