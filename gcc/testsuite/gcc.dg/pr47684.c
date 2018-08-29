/* PR debug/47684 */
/* { dg-do compile } */
/* { dg-options "-O3 -fcompare-debug" } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } } */

int in[8][4];
int out[4];

void
foo (void)
{
  int sum = 1;
  int i, j, k;
  for (k = 0; k < 4; k++)
    {
      for (j = 0; j < 4; j++)
	for (i = 0; i < 4; i++)
	  sum *= in[i + k][j];
      out[k] = sum;
    }
}
