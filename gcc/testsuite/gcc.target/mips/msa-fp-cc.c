/* { dg-do compile } */
/* { dg-options "-mabi=32 -mfp64 -mhard-float -mmsa" } */
typedef float v4f32 __attribute__((vector_size(16)));
typedef double v2f64 __attribute__((vector_size(16)));

v4f32
fcmpOeqVector4 (v4f32 a, v4f32 b)
{
  return a + b;
}

v2f64
fcmpOeqVector2 (v2f64 a, v2f64 b)
{
  return a + b;
}

/* { dg-final { scan-assembler-not "copy_s" } } */
/* { dg-final { scan-assembler-not "insert" } } */
