/* { dg-do compile } */
/* { dg-options "-O0" } */

extern __fp16 foo ();

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

/* { dg-final { scan-assembler-times {dup\tv[0-9]+.4h, w[0-9]+} 1 } } */
