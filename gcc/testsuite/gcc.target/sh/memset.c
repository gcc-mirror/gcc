/* Check that the __builtin_memset function is inlined when
   optimizing for speed.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } } */
/* { dg-final { scan-assembler-not "jmp" } } */

void
test00(char *dstb)
{
  __builtin_memset (dstb, 0, 15);
}

