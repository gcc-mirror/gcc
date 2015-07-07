/* { dg-do compile } */
/* { dg-options "-mno-smartmips" } */

NOCOMPRESSION int scaled_indexed_word_load (int a[], int b)
{
  return a[b];
}
/* { dg-final { scan-assembler-not "\tlwxs\t" } } */
