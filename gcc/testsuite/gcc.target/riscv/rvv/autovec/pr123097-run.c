/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d" } */

long long a;
int b[22];
_Bool c[22];

int main() {
    for (long f=0; f<10; ++f)
      b[f] = c[f] = 1;
    for (int f=0; f<5; f++)
      for( int g=0; g<5; g++)
        c[g*2] &= (_Bool)b[2*f];
    for (long f=0; f<2; ++f)
      a ^= c[f];
    if (a != 0)
      __builtin_abort ();
}
