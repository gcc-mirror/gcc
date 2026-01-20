/* { dg-do run } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -O3 -fsigned-char -fno-strict-aliasing -fwrapv -std=gnu99" { target rv64 } } */
/* { dg-options "-march=rv32gcv_zvl256b -mabi=ilp32 -O3 -fsigned-char -fno-strict-aliasing -fwrapv -std=gnu99" { target rv32 } } */
short a;
long long b;
char c[3][3][17];
_Bool d;

int main() {
  for (long g=0; g<3; ++g)
    for (long h=0; h<3; ++h)
      for (long i=0; i<17; ++i)
        c[g][h][i] = 2;

  for (char g=0; g<3; g-=13)
    for (short j=3; j<d+21; j+=2)
      a += 2;

  b = (int)a;
  if (b != 180)
    __builtin_abort ();
  __builtin_exit (0);
}
