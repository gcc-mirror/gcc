/* PR target/104923 */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Make sure we do not ICE on the following test cases.  */

void
foo (__vector char *dst, __vector_quad *acc, unsigned int n)
{
  __vector char a[4];
  __builtin_mma_disassemble_acc(a, acc);
  dst[2 * n] = a[0];
}

void
bar (__vector char *dst, __vector_quad *acc, unsigned int n)
{
  __vector char a[4];
  __builtin_mma_disassemble_acc(a, acc);
  dst[3 * n] = a[0];
}
