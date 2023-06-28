/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O2 -march=x86-64" } */
/* { dg-final { scan-assembler-times "rep mov" 1 } } */

__attribute__((target_clones("default","arch=icelake-server")))
void
foo (char *a, char *b, int size)
{
  __builtin_memcpy (a, b, size & 0x7F);
}
