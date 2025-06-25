/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic -mno-avx" } */
/* { dg-final { scan-assembler "jmp\tmemcpy" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\tmemcpy" { target ia32 } } } */
/* { dg-final { scan-assembler-not "rep movsb" } } */

void
foo (char *dest, char *src)
{
  __builtin_memcpy (dest, src, 257);
}
