/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_ok } */
/* { dg-add-options arm_v8_2a_fp16_scalar } */

extern __fp16 foo ();
extern void bar (__fp16* x);

void f1 ()
{
  volatile __fp16 a = 17.0;
}


void f2 (__fp16 *a)
{
  *a = 17.0;
}

void f3 ()
{
  __fp16 b = foo ();
  b = 17.0;
  bar (&b);
}

__fp16 f4 ()
{
  __fp16 a = 0;
  __fp16 b = 1;
  __fp16 c = 2;
  __fp16 d = 4;

  __fp16 z = a + b;
  z = z + c;
  z = z - d;
  return z;
}

__fp16 f5 ()
{
  __fp16 a = 16;
  bar (&a);
  return a;
}

/* { dg-final { scan-assembler-times "mov\tw\[0-9\]+, #?19520"           3 } } */
/* { dg-final { scan-assembler-times "movi\tv\[0-9\]+\\\.2s, 0xbc, lsl 8"  1 } } */
/* { dg-final { scan-assembler-times "movi\tv\[0-9\]+\\\.2s, 0x4c, lsl 8"  1 } } */
