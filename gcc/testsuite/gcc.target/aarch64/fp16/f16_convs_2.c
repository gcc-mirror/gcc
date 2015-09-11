/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mfp16-format=ieee" {target "arm*-*-*"} } */

extern void abort (void);

#define EPSILON 0.0001

int
main (int argc, char **argv)
{
  int i1 = 3;
  int i2 = 2;
  /*  This 'assembler' should be portable across ARM and AArch64.  */
  asm volatile ("" : : : "memory");

  __fp16 in1 = i1;
  __fp16 in2 = i2;

  /* Do the addition on __fp16's (implicitly converts both operands to
     float32, adds, converts back to f16, then we convert to int).  */
  __fp16 res1 = in1 + in2;
  asm volatile ("" : : : "memory");
  int result1 = res1;

  /* Do the addition on int's (we convert both operands directly to int, add,
     and we're done).  */
  int result2 = ((int) in1) + ((int) in2);

  if (__builtin_abs (result2 - result1) > EPSILON)
    abort ();
  return 0;
}
