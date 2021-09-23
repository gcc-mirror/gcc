/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ftree-vectorize -fno-vect-cost-model -fdump-tree-vect-details" } */

/* Check vect_recog_mulhs_pattern can be detected with shift count 32.  */

#define N 128

typedef signed long long sLL;
typedef unsigned long long uLL;

signed int si_a[N], si_b[N];
unsigned int ui_a[N], ui_b[N];
signed short sh_c[N];
unsigned short uh_c[N];

void
test1 ()
{
  for (int i = 0; i < N; i++)
    sh_c[i] = ((sLL) si_a[i] * (sLL) si_b[i]) >> 32;
}

void
test2 ()
{
  for (int i = 0; i < N; i++)
    uh_c[i] = ((uLL) ui_a[i] * (uLL) ui_b[i]) >> 32;
}

/* { dg-final { scan-tree-dump-times "vect_recog_mulhs_pattern: detected" 2 "vect" } } */
