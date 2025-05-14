/* PR target/119386 */
/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -fno-plt -pg" } */
/* { dg-final { scan-assembler "call\[ \t\]+\\*mcount@GOTPCREL\\(" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]+\\*mcount@GOT\\(" { target ia32 } } } */


int
main ()
{
  return 0;
}
