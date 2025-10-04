/* { dg-do compile } */
/* { dg-options "-mtune=generic-ooo -O3 -march=rv64gcv_zvl256b_zba -mabi=lp64d -mrvv-max-lmul=m2 -mrvv-vector-bits=scalable" } */

int test(int* in, int n)
{
  int accum = 0;
  for (int i = 0; i < n; i++)
        accum += in[i];

  return accum;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-z0-9]+,\s*[a-z0-9]+,\s*e[0-9]+,\s*m[f0-9]+,\s*ta,\s*ma} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-z0-9]+,\s*[a-z0-9]+,\s*e[0-9]+,\s*m[f0-9]+,\s*tu,\s*ma} 1 } } */

