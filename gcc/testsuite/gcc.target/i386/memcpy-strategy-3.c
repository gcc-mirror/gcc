/* { dg-do compile } */
/* { dg-options "-O2 -march=atom -mmemcpy-strategy=vector_loop:2000:align,libcall:-1:align" } */
/* On ELF platforms, one hit comes from the .file directive.  */
/* { dg-final { scan-assembler-times "memcpy" 2 { target { ! *-*-darwin* } } } } */
/* But not on Darwin, which doesn't have a .file directive by default.  */
/* { dg-final { scan-assembler-times "_memcpy" 1  { target *-*-darwin* } } } */

char a[2048];
char b[2048];
void t (void)
{
  __builtin_memcpy (a, b, 2048);
}
