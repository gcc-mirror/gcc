/* { dg-do compile } */
/* { dg-options "-O2 -march=atom -mmemset-strategy=libcall:-1:align" } */
/* On ELF platforms, one hit comes from the .file directive.  */
/* { dg-final { scan-assembler-times "memset" 2 { target { ! *-*-darwin* } } } } */
/* But not on Darwin, which doesn't have a .file directive by default.  */
/* { dg-final { scan-assembler-times "_memset" 1 { target *-*-darwin* } } } */

char a[2048];
void t (void)
{
  __builtin_memset (a, 1, 2048);
}
