/* Not to fuse widening multiply with accumulate if the multiply has more than
   one uses.
   Note that for targets where pointer and int are of the same size or
   widening multiply-and-accumulate is not available, this test just passes.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-widening_mul" } */

typedef int ArrT [10][10];

void
foo (ArrT Arr, int Idx)
{
  Arr[Idx][Idx] = 1;
  Arr[Idx + 10][Idx] = 2;
}

/* { dg-final { scan-tree-dump-not "WIDEN_MULT_PLUS_EXPR" "widening_mul" } } */
