/* { dg-do compile } */
/* { dg-options "-Wall -fwrapv" } */

long A[4], B[100];

void foo(void)
{
  int i, j, k = 3;
  while (A[k] && k > 0) k--; /* k = {0, 1, 2, 3} */
  for (i = 3 - k; i >= 0; i--) /* i = {0..3-k} */
    for (j = 0; j <= k; j++) { /* line 8; j = {0..k} */
	B[i + j] = 0; /* line 9; i + j = {0..3-k+k} = {0..3} */
	for (j = 0; j <= k; j++); /* only one iteration is done, with j == 0 */
    }
}

