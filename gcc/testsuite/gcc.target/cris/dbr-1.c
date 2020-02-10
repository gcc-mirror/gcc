/* Check that delayed-branch-slot is able to fill a trivially fillable
   slot.  The xfail is due to the "move.d [$r10+4],$r10" not being split
   up into "addq 4,$r10" and "move.d [$r10],$r10"; both slottable and of
   the same actual cost in size and cycles as the unsplit insn.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tnop" { xfail *-*-* } } } */
void *f(void **p)
{
  return p[1];
}
