/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options " -mfpu=fpv3" } */

int funce(__fp16 a)
{
  return (int)a;
}

unsigned int funcf(__fp16 a)
{
  return (unsigned int)a;
}

int funca(float a)
{
  return (int)a;
}

unsigned int funcb(float a)
{
  return (unsigned int)a;
}

int funcc(double a)
{
  return (int)a;
}

unsigned int funcd(double a)
{
  return (unsigned int)a;
}

/* { dg-final { scan-assembler "fftoi\.f16\.s32\.rz" } }*/
/* { dg-final { scan-assembler "fftoi\.f16\.u32\.rz" } }*/
/* { dg-final { scan-assembler "fftoi\.f32\.s32\.rz" } }*/
/* { dg-final { scan-assembler "fftoi\.f32\.u32\.rz" } }*/
/* { dg-final { scan-assembler "fftoi\.f64\.s32\.rz" } }*/
/* { dg-final { scan-assembler "fftoi\.f64\.u32\.rz" } }*/

