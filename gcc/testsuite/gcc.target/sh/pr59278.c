/* Check that combine considers unused regs dead.  */
/* { dg-do compile }  */
/* { dg-options "-O1" }  */
/* { dg-final { scan-assembler "addc" } }  */

struct result
{
  int a, b;
};

struct result
test_00 (int a, int b, int d)
{
  struct result r;
  r.a = a != b;
  r.b = d + b + 1;
  return r;
}
