/* Verify signed division does not produce 'sdiv' insn in eBPF.  */
/* { dg-do compile } */
/* { dg-options "-O0 -mcpu=v3" } */

void
foo ()
{
  signed int x = 5;
  signed int y = 2;
  signed int z = x / y;
}
/* { dg-final { scan-assembler-not "sdiv(32)?\t%r" } } */
