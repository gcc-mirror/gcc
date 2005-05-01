/* Test if we inline memcpy even with -Os, when the user requested it.  */
/* Don't name this test with memcpy in its name, otherwise the scan-assembler
   would be confused.  */
/* { dg-do compile { target i?86-*-linux* x86_64-*-linux* } } */
/* { dg-options "-Os -minline-all-stringops" } */
/* { dg-final { scan-assembler-not "memcpy" } } */
char f(int i)
{
  char *ram_split[] = { "5:3", "3:1", "1:1", "3:5" };
  return ram_split[i][0];
}
