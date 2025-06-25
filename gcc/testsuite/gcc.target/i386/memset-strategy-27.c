/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic -mno-avx" } */
/* { dg-final { scan-assembler "jmp\tmemset" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\tmemset" { target ia32 } } } */
/* { dg-final { scan-assembler-not "rep stosb" } } */

void
foo (char *dest)
{
  __builtin_memset (dest, 0, 257);
}
