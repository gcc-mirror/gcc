/* { dg-do run } */
/* { dg-require-effective-target riscv_v } */
/* { dg-options { -O3 -fno-vect-cost-model -march=rv64gcv -mabi=lp64d  } } */

char a;
int b;
short e[14];
char f[4][12544];
_Bool c[4][5];

__attribute__ ((noipa))
void foo (int a)
{
  if (a != 1)
    __builtin_abort ();
}

int main ()
{
  for (int i = 0; i < 4; ++i)
    for (int l = 0; l < 15; ++l)
      for (int m = 0; m < 15; ++m)
	f[i][l * m] = 3;
  for (int j = 0; j < 4; j += 1)
    for (int k = 3; k < 13; k += 3)
      for (_Bool l = 0; l < 1; l = 1)
	for (int m = 0; m < 4; m += 1)
	  {
	    a = 0;
	    b -= e[k];
	    c[j][m] = f[j][6];
	  }
  for (long i = 2; i < 4; ++i)
    foo (c[3][3]);
}
