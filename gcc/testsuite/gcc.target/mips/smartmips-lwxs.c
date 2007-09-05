/* { dg-do compile } */
/* { dg-mips-options "-O -msmartmips" } */

#define NOMIPS16 __attribute__ ((nomips16)) 

NOMIPS16 int scaled_indexed_word_load (int a[], int b)
{
  return a[b];
}
/* { dg-final { scan-assembler "\tlwxs\t" } } */
