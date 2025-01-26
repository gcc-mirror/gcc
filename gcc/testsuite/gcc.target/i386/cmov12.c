/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx2 -mtune=sapphirerapids" } */                             
/* { dg-final { scan-assembler-times "cmov(\[lq\]\.)?g" 3 } } */

void foo(int *a, int n, int k)
{
  int j, v;

  v = a[k - 1];
  while (k <= n / 2) {
      j = k + k;
      if ((j < n) && (a[j - 1] < a[j]))
	j++;
      if (v >= a[j - 1])
	break;
      a[k - 1] = a[j - 1];
      k = j;
  }
  a[k - 1] = v;
}

