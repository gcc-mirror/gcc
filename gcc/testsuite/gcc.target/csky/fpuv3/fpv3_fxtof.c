/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck860f"  { csky-*-* }  { "*" }  { "-mcpu=ck860*f* -mfloat-abi=hard" "-mcpu=ck860*f* -mhard-float"  }  }  */
/* { dg-options "-O1" } */

//__fp16
//fixed32_to_float16 (int i)
//{
//  return ((__fp16) i / (1 << 15));
//}
//
//__fp16
//fixedu32_to_float16 (unsigned int i)
//{
//  return ((__fp16) i / (1 << 15));
//}
//
//float
//fixed32_to_float32 (int i)
//{
//  return ((float) i / (1 << 30));
//}
//
//
//float
//fixedu32_to_float32 (unsigned int i)
//{
//  return ((float) i / (1 << 30));
//}
//
//double
//fixed32_to_float64 (int i)
//{
//  return ((double) i / (1 << 30));
//}
//
//double
//fixedu32_to_float64 (unsigned int i)
//{
//  return ((double) i / (1 << 30));
//}
//
//__fp16
//fixed16_to_float16 (short i)
//{
//  return ((__fp16) i / (1 << 15));
//}
//
//__fp16
//fixedu16_to_float16 (unsigned short i)
//{
//  return ((__fp16) i / (1 << 15));
//}
//
//float
//fixed16_to_float32 (short i)
//{
//  return ((float) i / (1 << 16));
//}
//
//float
//fixedu16_to_float32 (unsigned short i)
//{
//  return ((float) i / (1 << 16));
//}
//
//double
//fixed16_to_float64 (short i)
//{
//  return ((double) i / (1 << 16));
//}
//
//double
//fixedu16_to_float64 (unsigned short i)
//{
//  return ((double) i / (1 << 16));
//}
