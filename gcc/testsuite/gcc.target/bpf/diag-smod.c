/* Verify signed modulo does not produce 'smod' insn in eBPF.  */
/* { dg-do compile } */
/* { dg-options "-O0" } */

void
foo ()
{
  signed int x = 5;
  signed int y = 2;
  signed int z = x % y;
}
/* { dg-final { scan-assembler-not "smod(32)?\t%r" } } */
