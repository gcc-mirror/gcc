/* PR debug/51410 */
/* { dg-do compile } */
/* { dg-options "-O0 -gdwarf -dA -fno-merge-debug-strings" } */

int x;

int
foo (void)
{
  return x;
}

/* { dg-final { scan-assembler-times "\\(DIE\[^\\r\\n\]*DW_TAG_variable\\)" 1 } } */
