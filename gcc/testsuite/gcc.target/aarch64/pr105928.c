/* { dg-do assemble } */
/* { dg-options "-O2 --save-temps" } */

long long f1 (void)
{
  return 0x80402010080400;
}

long long f2 (void)
{
  return 0x1234567812345678;
}

long long f3 (void)
{
  return 0x4567800012345678;
}

long long f4 (void)
{
  return 0x3ecccccd3ecccccd;
}

long long f5 (void)
{
  return 0x38e38e38e38e38e;
}

long long f6 (void)
{
  return 0x1745d1745d1745d;
}

void f7 (long long *p)
{
  *p = 0x1234567812345678;
}

/* { dg-final { scan-assembler-times {\tmovk\t} 7 } } */
/* { dg-final { scan-assembler-times {\tmov\t} 7 } } */
/* { dg-final { scan-assembler-times {\tbic\t} 2 } } */
/* { dg-final { scan-assembler-times {\torr\t} 4 } } */
/* { dg-final { scan-assembler-times {\tstp\t} 1 } } */
