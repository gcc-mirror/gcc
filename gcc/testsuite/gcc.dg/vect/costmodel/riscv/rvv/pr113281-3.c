/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=m8" } */

unsigned char a;

int main() {
  short b = a = 0;
  for (; a != 19; a++)
    if (a)
      b = 32872 >> a;

  if (b == 0)
    return 0;
  else
    return 1;
}

/* { dg-final { scan-assembler-not {vset} } } */
