/* { dg-require-effective-target rop_ok } */
/* { dg-options "-O1 -mrop-protect -mdejagnu-cpu=power10" } */

extern void foo (void);

long int
__attribute__ ((__optimize__ ("no-inline")))
func (long int cond)
{
  if (cond)
    foo ();
  return cond;
}

/* Ensure hashst comes after mflr and hashchk comes after ld 0,16(1).  */
/* { dg-final { scan-assembler {(?p)\mmflr 0.*\n.*\n.*\mhashst 0,} } } */
/* { dg-final { scan-assembler {(?p)ld 0,.*\n.*\n.*\n.*\mhashchk 0,} } } */
