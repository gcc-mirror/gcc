/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake -mno-sse" } */
/* { dg-final { scan-assembler "jmp\t_?memset" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\t_?memset" { target ia32 } } } */
/* { dg-final { scan-assembler-not "rep stosb" } } */

void
foo (char *dest)
{
  __builtin_memset (dest, 0, 257);
}
