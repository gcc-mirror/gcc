/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

int __attribute__ ((vector_size (32))) x;
void
foo (void)
{
  x <<= x;
}
