/* PR target/59874 */
/* { dg-do compile } */
/* { dg-options "-O2 -mpopcnt -masm=att" } */
/* { dg-final { scan-assembler "\tpopcntw" { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler "\tpopcnt" { target *-*-darwin* } } } */

unsigned int
foo (unsigned short x)
{
  return __builtin_popcount (x);
}
