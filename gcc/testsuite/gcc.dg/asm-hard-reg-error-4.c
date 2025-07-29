/* { dg-do compile } */

/* Verify output operands.  */

int
test (void)
{
  int x;
  register int y __asm__ ("0");

  /* Preserve status quo and don't error out.  */
  __asm__ ("" : "=r" (x), "=r" (x));

  /* Be more strict for hard register constraints and error out.  */
  __asm__ ("" : "={0}" (x), "={1}" (x)); /* { dg-error "multiple outputs to lvalue 'x'" } */

  /* Still error out in case of a mixture.  */
  __asm__ ("" : "=r" (x), "={1}" (x)); /* { dg-error "multiple outputs to lvalue 'x'" } */

  return x + y;
}
