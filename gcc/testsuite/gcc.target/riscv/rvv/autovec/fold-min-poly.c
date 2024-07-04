/* { dg-do compile } */
/* { dg-options " -march=rv64gcv_zvl128b -mabi=lp64d -O3 -mrvv-vector-bits=scalable -mrvv-max-lmul=m1" } */

void foo1 (int* restrict a, int* restrict b, int n)
{
    for (int i = 0; i < 4; i += 1)
      a[i] += b[i];
}

void foo2 (int* restrict a, int* restrict b, int n)
{
    for (int i = 0; i < 3; i += 1)
      a[i] += b[i];
}

void foo3 (int* restrict a, int* restrict b, int n)
{
    for (int i = 0; i < 5; i += 1)
      a[i] += b[i];
}

/* { dg-final { scan-assembler-not {\tcsrr\t} } } */
/* { dg-final { scan-assembler {\tvsetivli\tzero,4,e32,m1,t[au],m[au]} } } */
/* { dg-final { scan-assembler {\tvsetivli\tzero,3,e32,m1,t[au],m[au]} } } */
