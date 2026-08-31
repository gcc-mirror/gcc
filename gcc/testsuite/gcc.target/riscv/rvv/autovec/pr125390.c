/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -mrvv-vector-bits=zvl -O2 --param=riscv-autovec-mode=RVVMF8QI -fno-vect-cost-model" } */

int a, b[64], c[64];

void
foo (void)
{
  for (unsigned e = 0; e < 8; e += 2)
    {
      a = c[e] ^ c[e + 1];
      b[e] = 0;
    }
}

void
bar (void)
{
  for (unsigned e = 0; e < 16; e += 2)
    {
      a = c[e] ^ c[e + 1];
      b[e] = 0;
    }
}

void
baz (void)
{
  for (unsigned e = 0; e < 32; e += 4)
    {
      a = c[e] ^ c[e + 1] ^ c[e + 2] ^ c[e + 3];
      b[e] = 0;
    }
}
