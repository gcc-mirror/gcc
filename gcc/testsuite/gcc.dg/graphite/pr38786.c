/* { dg-options "-O2 -fgraphite-identity" } */

typedef struct
{
  int ****cofAC;
} ImageParameters;
typedef struct
{
  int ****cofAC;
} RD_DATA;
extern RD_DATA *rdopt;
extern ImageParameters *img;
void
dummy_slice_too_big (int bits_slice)
{
  int i, j, k, l;
  for (j = 0; j < 4; j++)
    for (k = 0; k < 2; k++)
      for (l = 0; l < 65; l++)
	img->cofAC[i][j][k][l] = rdopt->cofAC[i][j][k][l];
}
