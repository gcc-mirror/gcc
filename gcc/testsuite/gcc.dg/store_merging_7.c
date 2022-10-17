/* { dg-do compile } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-store-merging" } */

/* Check that we can merge consecutive array members through the pointer.
   PR rtl-optimization/23684.  */

void
foo (char *input)
{
  input = __builtin_assume_aligned (input, 8);
  input[0] = 'H';
  input[1] = 'e';
  input[2] = 'l';
  input[3] = 'l';
  input[4] = 'o';
  input[5] = ' ';
  input[6] = 'w';
  input[7] = 'o';
  input[8] = 'r';
  input[9] = 'l';
  input[10] = 'd';
  input[11] = '\0';
}

/* { dg-final { scan-tree-dump-times "Merging successful" 1 "store-merging" } } */
