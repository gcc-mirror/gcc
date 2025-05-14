/* PR target/119386 */
/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fpic -pg -mnop-mcount" } */
/* { dg-final { scan-assembler ".byte\[ \t\]+0x0f, 0x1f, 0x44, 0x00, 0x00" } } */

int
main ()
{
  return 0;
}
