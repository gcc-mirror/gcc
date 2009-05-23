/* Do not use 16-bit multiply instructions in Thumb-2 mode when
   optimizing for speed.  */
/* { dg-options "-mthumb -O2" }  */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-final { scan-assembler-not "muls" } } */

int f(int i, int j) 
{
  return i * j;
}

int x;

void g(int i, int j)
{
  if (i * j < 0)
    x = 1;
}

int h(int i, int j)
{
  i = i * j;
  if (i < 0)
    x = 1;
  return i;
}

