/* { dg-do compile } */
/* { dg-require-effective-target pie } */
/* { dg-options "-O2 -fPIE -pie" } */

int look_nbits[256], loop_sym[256];
const int ind[] = {
  0,  1,  8, 16,  9,  2,  3, 10, 17, 24, 32, 25, 18, 11,  4,  5,
 12, 19, 26, 33, 40, 48, 41, 34, 27, 20, 13,  6,  7, 14, 21, 28,
 35, 42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51,
 58, 59, 52, 45, 38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63
};
int out[256];
extern void bar (int *, int *);
void foo (int *l1, int *l2, int *v, int *v1, int *m1, int i)
{
  int L = i + 1, b = 20;
  int result, k;

  for (k = 1; k < 64; k++)
    {
      int look = (((L >> (b - 8))) & ((1 << 8) - 1));
      int nb = l1[look];
      int code;
      int r;

      if (nb)
	{
	  b -= nb;
	  result = l2[look];
	}
      else
	{
	  nb = 9;
	  code = (((L >> (b -= nb))) & ((1 << nb) - 1));
	  result = v[(code + v1[nb])];
	}
      r = result >> 4;
      result &= 15;
      if (result)
	{
	  k += r;
	  r = (((L >> (b -= result))) & ((1 << result) - 1));
	  if (r < (1 << (result - 1)))
	    result = r + (((-1) << result) + 1);
	  else
	    result = r;

	  out[ind[k]] = result;
	}
      bar (&L, &b);
    }
}

/* { dg-final { scan-assembler-not "leal\[ \t\]ind@GOTOFF\\(%\[^,\]*\\), %" { target ia32 } } } */
