/* { dg-options "(HAS_LXC1) -mhard-float -mno-lxc1-sxc1" } */
/* { dg-final { scan-assembler-not "\tldxc1\t" } } */
/* { dg-final { scan-assembler-not "\tsdxc1\t" } } */

#ifndef __mips_no_lxc1_sxc1
#error missing definition of __mips_no_lxc1_sxc1
#endif

double ldexp(double x, int exp);

typedef struct
{
  double** rows;
} d_mat_struct;

typedef d_mat_struct d_mat_t[1];

#define d_mat_entry(mat,i,j) (*((mat)->rows[i] + (j)))

double __attribute__((noinline))
ldxc1_test (int kappa, int zeros, double ctt, int* expo, d_mat_t r, double* s)
{
  int kappa2 = kappa;
  double tmp = 0.0;

  do
    {
      kappa--;
      if (kappa > zeros + 1)
	{
	  tmp = d_mat_entry(r, kappa - 1, kappa - 1) * ctt;
	  tmp = ldexp(tmp, (expo[kappa - 1] - expo[kappa2]));
	}
    }
  while ((kappa >= zeros + 2) && (s[kappa - 1] <= tmp));

  return tmp;
}

#define SIZE 20

int main(void)
{
  int kappa = SIZE - 1;
  int zeros = 1;
  double ctt = 2;

  int expo[SIZE] = {0};
  double s[SIZE] = {0};
  double rows_data[SIZE][SIZE] = {0};
  double* rows[SIZE];

  for (int i = 0; i < SIZE; i++)
    rows[i] = rows_data[i];

  d_mat_t r = { rows };

  ldxc1_test(kappa, zeros, ctt, expo, r, s);
  return 0;
}
