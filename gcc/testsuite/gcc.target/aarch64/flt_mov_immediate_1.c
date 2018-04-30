/* { dg-do compile } */
/* { dg-options "-O3" } */

float f0(void)
{
  float x = 0.0f;
  return x;
}

float fn1(void)
{
  float x = -0.0f;
  return x;
}

float f1(void)
{
  float x = 256.0f;
  return x;
}

float f2(void)
{
  float x = 123256.0f;
  return x;
}

float f3(void)
{
  float x = 2.0f;
  return x;
}

float f4(void)
{
  float x = -20000.1;
  return x;
}


/* { dg-final { scan-assembler-times "movi\tv\[0-9\]+\\\.2s, ?#0"           1 } } */
/* { dg-final { scan-assembler-times "movi\tv\[0-9\]+\\\.2s, 0x80, lsl 24"  1 } } */
/* { dg-final { scan-assembler-times "movi\tv\[0-9\]+\\\.2s, 0x80, lsl 24"  1 } } */

/* { dg-final { scan-assembler-times "mov\tw\[0-9\]+, 48128"                1 } } */
/* { dg-final { scan-assembler-times "movk\tw\[0-9\]+, 0x47f0, lsl 16"      1 } } */

/* { dg-final { scan-assembler-times "fmov\ts\[0-9\]+, 2\\\.0e\\\+0"  1 } } */

/* { dg-final { scan-assembler-times "mov\tw\[0-9\]+, 16435"                1 } } */
/* { dg-final { scan-assembler-times "movk\tw\[0-9\]+, 0xc69c, lsl 16"      1 } } */

