/* { dg-do compile } */
/* { dg-options "-ftree-loop-if-convert-stores" } */

int a, b;
float xsum[100];
void foo (float *cluster)
{
  int j;
  for (; a ; ++j) {
      xsum[j] = cluster[j];
      if (xsum[j] > 0)
	xsum[j] = 0;
  }
  if (xsum[0])
    b = 0;
}
