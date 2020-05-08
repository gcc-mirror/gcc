/* PR target/94857 */
/* { dg-do compile } */
/* { dg-options "-O2 -mtune=skylake -masm=att" } */
/* { dg-additional-options "-mregparm=2" { target ia32 } } */
/* { dg-final { scan-assembler "\taddl\t%\[a-z0-9]\*, \\\(" } } */

int
foo (unsigned *p, unsigned x)
{
  unsigned u = *p;
  *p += x;
  return u > *p;
}
