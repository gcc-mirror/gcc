/* PR target/111698 */
/* { dg-options "-O2 -masm=att -march=sandybridge" } */
/* { dg-final { scan-assembler-not "testl" } }  */

int m;

_Bool foo (void)
{
  return m & 0x0a0000;
}

/* { dg-final { scan-assembler-times "testb" 1 } }  */

_Bool bar (void)
{
  return m & 0xa0a000;
}

/* { dg-final { scan-assembler-times "testw" 1 } }  */
