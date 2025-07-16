/* PR target/119386 */
/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -pg" } */
/* { dg-additional-options "-mfentry" { target { *-*-gnu* && { ! ia32 } } } } */
/* { dg-final { scan-assembler "call\[ \t\]+mcount@PLT" { target ia32 } } } */
/* { dg-final { scan-assembler "call\[ \t\]+__fentry__@PLT" { target { *-*-gnu* && { ! ia32 } } } } } */

int
main ()
{
  return 0;
}
