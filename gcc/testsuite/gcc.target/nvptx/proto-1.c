/* { dg-do compile } */

int f(void)
{
  const int dev = 4;

  /* Check that without an explicit prototype, we deduce from call site the
     signature for the (mandatory in PTX) prototype.  */
  /* extern int foo (int *); */
  /* { dg-final { scan-assembler-not "\\\.callprototype" } } */
  /* { dg-final { scan-assembler "\\\.extern \\\.func \\\(\[^,\n\r\]+\\\) foo \\\(\[^,\n\r\]+\\\);" } } */
  return !foo(&dev);
}
