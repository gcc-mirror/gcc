/* { dg-do compile } */
/* { dg-options "-O2 -march=tigerlake -mno-sse" } */
/* { dg-final { scan-assembler "jmp\t_?memcpy" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\t_?memcpy" { target ia32 } } } */
/* { dg-final { scan-assembler-not "rep movsb" } } */

void
foo (char *dest, char *src)
{
  __builtin_memcpy (dest, src, 257);
}
