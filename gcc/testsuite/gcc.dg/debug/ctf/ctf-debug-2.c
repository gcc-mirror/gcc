/* Verify that CTF debug info can co-exist with dwarf.  */
/* { dg-do compile } */
/* { dg-options "-gdwarf -gctf -dA" } */
/* { dg-final { scan-assembler "0xdff2.*CTF preamble magic number" } } */

void func (void)
{ }
