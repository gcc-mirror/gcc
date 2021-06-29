/* Verify the CTF preamble in the CTF section.  */

/* { dg-do compile )  */
/* { dg-options "-O0 -gctf -dA" } */
/* { dg-final { scan-assembler "0xdff2.*CTF preamble magic number" } } */
/* { dg-final { scan-assembler "0x4.*CTF preamble version" } } */
/* { dg-final { scan-assembler "0.*CTF preamble flags" } } */

void func (void)
{
}
