/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fcaller-saves -ffixed-d8 -ffixed-d9 -ffixed-d10 -ffixed-d11 -ffixed-d12 -ffixed-d13 -ffixed-d14 -ffixed-d15" } */

/* Under high register pressure FP immediates should be rematerialized
   as literal loads rather than being caller-saved to the stack.  */

void
g (void);

float
f (float x)
{
  x += 3.1f;
  g ();
  x *= 3.1f;
  return x;
}

/* { dg-final { scan-assembler-times "ldr\ts\[0-9]+, .LC0" 2 } } */

