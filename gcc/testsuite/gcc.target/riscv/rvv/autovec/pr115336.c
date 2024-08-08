/* { dg-do run } */
/* { dg-options { -O3 -march=rv64gcv_zvl256b -mabi=lp64d } } */
/* { dg-require-effective-target rvv_zvl256b_ok } */

short d[19];
_Bool e[100][19][19];
_Bool f[10000];

int main()
{
  for (long g = 0; g < 19; ++g)
    d[g] = 3;
  _Bool(*h)[19][19] = e;
  for (short g = 0; g < 9; g++)
    for (int i = 4; i < 16; i += 3)
      f[i * 9 + g] = d[i] ? d[i] : h[g][i][2];
  for (long i = 120; i < 122; ++i)
    if (f[i] != 1)
      __builtin_abort ();
}
