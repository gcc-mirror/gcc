/* { dg-do run } */
/* { dg-require-effective-target rvv_zvl512b_ok } */
/* { dg-options "-O3 -march=rv64gcv_zvl512b -mabi=lp64d -mrvv-vector-bits=zvl -fsigned-char" } */
unsigned e[2][2];
long a;
char c[2];

int
main ()
{
  long long b;
  c[1] = 3;
  for (unsigned h = 0; h < 2; h++)
    for (int i = c[0]; i < 5; i += 5)
      for (int j = 0; j < 219; j++)
	a = c[h] ? e[h][h] + 3326195747 : 0;

  b = a;
  if (b != 3326195747)
    __builtin_abort ();
}
