/* Ensure mempcpy is not "optimized" into memcpy followed by addition.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void *
fn (char *x, char *y, int z)
{
  return __builtin_mempcpy (x, y, z);
}

/* { dg-final { scan-assembler-not "memcpy" } } */
