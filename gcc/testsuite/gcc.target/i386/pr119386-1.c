/* PR target/119386 */
/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -pg" } */
/* { dg-final { scan-assembler "call\[ \t\]+mcount@PLT" } } */

int
main ()
{
  return 0;
}
