/* PR target/85073 */
/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -fdisable-tree-sccp" } */

int
foo (unsigned x)
{
  int c = 0;
  while (x)
    {
      c += 1;
      x = (x - 1) & x;
    }

  return c;
}

/* { dg-final { scan-assembler-times "test" 1 } } */
