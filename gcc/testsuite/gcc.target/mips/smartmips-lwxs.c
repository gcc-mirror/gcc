/* { dg-do compile } */
/* -mlong32 added because of PR target/38599.  */
/* { dg-options "-O -msmartmips -mlong32" } */

NOMIPS16 int scaled_indexed_word_load (int a[], int b)
{
  return a[b];
}
/* { dg-final { scan-assembler "\tlwxs\t" } } */
