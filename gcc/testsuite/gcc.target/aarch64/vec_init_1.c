/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer --save-temps -fno-inline" } */

extern void abort (void);

typedef float float16x4_t __attribute__ ((vector_size ((16))));

float a;
float b;

float16x4_t
make_vector ()
{
  return (float16x4_t) { 0, 0, a, b };
}

int
main (int argc, char **argv)
{
  a = 4.0;
  b = 3.0;
  float16x4_t vec = make_vector ();
  if (vec[0] != 0 || vec[1] != 0 || vec[2] != a || vec[3] != b)
    abort ();
  return 0;
}

/* For memory models that don't have an addend on the lane value
   load we can merge the load and lane insert into an LD1.
   For others we expect LDR + INS sequences.  */
/* { dg-final { scan-assembler-times "ld1\\t" 2 { target { aarch64_tiny || aarch64_large } } } } */
/* { dg-final { scan-assembler-times "ins\\t" 2 { target aarch64_small } } } */
/* What we want to check, is that make_vector does not stp the whole vector
   to the stack.  Unfortunately here we scan the body of main() too, which may
   be a bit fragile - the test is currently passing only because of the option
   -fomit-frame-pointer which avoids use of stp in the prologue to main().  */
/* { dg-final { scan-assembler-not "stp\\t" } } */
