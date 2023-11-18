/* Loading a register with constant 0 integer value.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

int
test_set_0_si (void)
{
  /* Since zero-extension is free, "zero" fill is not implemented for SI.  */
  /* { dg-final { scan-assembler "ldi\\tr14(.b0)?, 0" } } */
  return 0;
}

long long
test_set_0_di (void)
{
  /* { dg-final { scan-assembler "zero\\tr14(.b0)?, 8" } } */
  return 0;
}
