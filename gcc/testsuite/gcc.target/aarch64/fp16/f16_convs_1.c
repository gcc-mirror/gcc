/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mfp16-format=ieee" {target "arm*-*-*"} } */

extern void abort (void);

#define EPSILON 0.0001

int
main (int argc, char **argv)
{
  float f1 = 3.14159f;
  float f2 = 2.718f;
  /* This 'assembler' statement should be portable between ARM and AArch64.  */
  asm volatile ("" : : : "memory");
  __fp16 in1 = f1;
  __fp16 in2 = f2;

  /* Do the addition on __fp16's (implicitly converts both operands to
     float32, adds, converts back to f16, then we convert back to f32).  */
  __fp16 res1 = in1 + in2;
  asm volatile ("" : : : "memory");
  float f_res_1 = res1;

  /* Do the addition on float32's (we convert both operands to f32, and add,
     as above, but skip the final conversion f32 -> f16 -> f32).  */
  float f1a = in1;
  float f2a = in2;
  float f_res_2 = f1a + f2a;

  if (__builtin_fabs (f_res_2 - f_res_1) > EPSILON)
    abort ();
  return 0;
}
