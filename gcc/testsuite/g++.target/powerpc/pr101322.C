/* PR target/101322 */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power10" } */

/* Verify we don't ICE on the following test cases.  */

void
foo (char *resp, char *vpp)
{
  __builtin_vsx_disassemble_pair (resp, (__vector_pair *) vpp);
}

void
bar (char *resp, char *vpp)
{
  __builtin_mma_disassemble_acc (resp, (__vector_quad *)vpp);
}
