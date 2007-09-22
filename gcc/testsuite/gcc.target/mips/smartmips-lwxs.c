/* { dg-do compile } */
/* { dg-mips-options "-O -msmartmips" } */

NOMIPS16 int scaled_indexed_word_load (int a[], int b)
{
  return a[b];
}
/* { dg-final { scan-assembler "\tlwxs\t" } } */
