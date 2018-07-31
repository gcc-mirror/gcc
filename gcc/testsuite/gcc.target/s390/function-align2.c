/* { dg-do compile } */
/* { dg-options "-Os -march=z13"  } */

void bar ()
{
  /* { dg-final { scan-assembler-times ".align\t8" 2 } } */
}

__attribute__((optimize("O2")))
void baz ()
{
  /* { dg-final { scan-assembler-times ".align\t16" 1 } } */
}
