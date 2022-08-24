/* Loading a register with constant -1 integer value.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

int
test_set_m1_si (void)
{
  /* { dg-final { scan-assembler "fill\\tr14(.b0)?, 4" } } */
  return -1;
}

long long
test_set_m1_di (void)
{
  /* { dg-final { scan-assembler "fill\\tr14(.b0)?, 8" } } */
  return -1;
}
