/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O3 -mdejagnu-cpu=power7 -misel -ffast-math" } */

struct phylo_s {
  int left;
};
void Free2DArray (void **, int);

int Cluster(float **dmx, int N, struct phylo_s *tree)
{
  float **mx;
  int *coord;
  int i;
  int Np;
  int row, col;
  float min;
  for (col = 0; col < N; Np--)
    {
      for (row = 0; row < Np; row++)
	for (col = row+1; col < Np; col++)
	  if (mx[row][col] < min)
	    i = row;
      tree[Np-2].left = coord[i];
    }
  Free2DArray((void **) mx, N);
}
