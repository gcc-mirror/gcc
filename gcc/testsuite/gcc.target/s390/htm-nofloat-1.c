/* { dg-do compile } */
/* { dg-options "-O3 -march=zEC12 -mzarch" } */

int
foo ()
{
  __builtin_tbegin_nofloat (0);
  __builtin_tbegin_retry_nofloat (0, 42);
}
/* Make sure no FPR saves/restores are emitted.  */
/* { dg-final { scan-assembler-not "std" } } */
/* { dg-final { scan-assembler-not "ld" } } */
