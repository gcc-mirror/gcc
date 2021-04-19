/* PR target/100075 */
/* { dg-do compile } */
/* { dg-require-effective-target aarch64_little_endian } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not {\tsbfx\tx[0-9]+, x[0-9]+, 16, 16} } } */
/* { dg-final { scan-assembler {\tneg\tw[0-9]+, w[0-9]+, asr 16} } } */
/* { dg-final { scan-assembler {\textr\tw[0-9]+, w[0-9]+, w[0-9]+, 16} } } */

struct S { short x, y; };

struct S
f1 (struct S p)
{
  return (struct S) { -p.y, p.x };
}

struct S
f2 (struct S p)
{
  return (struct S) { p.y, -p.x };
}
