/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-additional-options "-march=armv8.2-a+sve -fdump-tree-ifcvt -Ofast" { target { aarch64*-*-* } } } */

extern int __attribute__ ((simd, const)) fn (float);

const int N = 20;
const float lim = 101.0;
const float cst =  -1.0;
float tot =   0.0;

float b[20];
float a[20] = { [0 ... 9] = 1.7014118e39, /* If branch. */
		[10 ... 19] = 100.0 };    /* Else branch.  */

int main (void)
{
  #pragma omp simd
  for (int i = 0; i < N; i += 1)
    {
      if (a[i] > lim)
	b[i] = cst;
      else
	b[i] = fn (a[i]);
      tot += b[i];
    }
  return (0);
}

/* { dg-final { scan-tree-dump {(_\d+) = (_\d+) > 1.01e\+2;\n\s*(_\d+) = ~\1;\n\s*_\d+ = .MASK_CALL \(fn, \2, \3\);} ifcvt } } */
