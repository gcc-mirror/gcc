/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2" } */
/* { dg-final { scan-assembler-not "slli.w\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,0" } } */

struct pmop
{
  unsigned int op_pmflags;
  unsigned int op_pmpermflags;
};
unsigned int PL_hints;

struct pmop *pmop;
void
Perl_newPMOP (int type, int flags)
{
  if (PL_hints & 0x00100000)
    pmop->op_pmpermflags |= 0x0001;
  if (PL_hints & 0x00000004)
    pmop->op_pmpermflags |= 0x0800;
  pmop->op_pmflags = pmop->op_pmpermflags;
}
