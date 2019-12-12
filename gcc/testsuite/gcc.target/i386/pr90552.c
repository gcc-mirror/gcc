/* PR target/90552 *
/* { dg-do compile } */
/* { dg-options "-Os" } */

__attribute__((optimize(2)))
int foo (int x)
{
  return x / 3;
}

/* { dg-final { scan-assembler-not "idiv" } } */
