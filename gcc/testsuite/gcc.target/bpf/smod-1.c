/* Verify that smod instruction is used for xBPF. */
/* { dg-do compile } */
/* { dg-options "-O0 -mcpu=v4 -masm=normal" } */

void
foo ()
{
  signed int x = 5;
  signed int y = 2;
  signed int z = x % y;
  signed int w = x % 3;
}

/* { dg-final { scan-assembler "smod(32)?\t%r" } } */
