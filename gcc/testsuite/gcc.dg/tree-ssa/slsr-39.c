/* Verify straight-line strength reduction for back-tracing
   CAND_ADD for T2 in:

    *PBASE:    T1
    *POFFSET:  MULT_EXPR (T2, C3)
    *PINDEX:   C1 + (C2 * C3) + C4  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-slsr-details" } */

typedef int arr_2[50][50];

void foo (arr_2 a2, int v1)
{
  int i, j;

  i = v1 + 5;
  j = i;
  a2 [i] [j++] = i;
  a2 [i] [j++] = i;
  a2 [i] [i-1] += 1;
  return;
}

/* { dg-final { scan-tree-dump-times "Replacing reference: " 4 "slsr" } } */
