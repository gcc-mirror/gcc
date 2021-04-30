/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-mfpu=fpv3" } */

__fp16 funce(int a)
{
  return (__fp16)a;
}

__fp16 funcf(unsigned int a)
{
  return (__fp16)a;
}

float funca(int a)
{
  return (float)a;
}

float funcb(unsigned int a)
{
  return (float)a;
}

double funcc(int a)
{
  return (double)a;
}

double funcd(unsigned int a)
{
  return (double)a;
}

//double funch(short a)
//{
//  return (double)a;
//}
//
//double funci(unsigned short a)
//{
//  return (double)a;
//}
//
//float funcj(short a)
//{
//  return (float)a;
//}
//
//float funck(unsigned short a)
//{
//  return (float)a;
//}

__fp16 funcm(short a)
{
  return (__fp16)a;
}

__fp16 funcn(unsigned short a)
{
  return (__fp16)a;
}

/* { dg-final { scan-assembler "fitof\.s32\.f16" } }*/
/* { dg-final { scan-assembler "fitof\.u32\.f16" } }*/
/* { dg-final { scan-assembler "fitof\.s32\.f32" } }*/
/* { dg-final { scan-assembler "fitof\.u32\.f32" } }*/
/* { dg-final { scan-assembler "fitof\.s32\.f64" } }*/
/* { dg-final { scan-assembler "fitof\.u32\.f64" } }*/
/* { dg-final { scan-assembler "fitof\.s16\.f16" } }*/
/* { dg-final { scan-assembler "fitof\.u16\.f16" } }*/
