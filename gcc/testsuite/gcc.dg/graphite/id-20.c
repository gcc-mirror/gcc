/* { dg-options "-O3 -fgraphite-identity -ffast-math" } */

typedef enum
{
  I_SLICE,
} SliceType;
typedef struct
{
  int type;
} ImageParameters;
extern ImageParameters *img;
int A[64], B[64], C[13][8][8], D[13][8][8];

void
foo (int q, int temp)
{
  int i, j, k;
  for(k=0; k<13; k++)
    for(j=0; j<8; j++)
      for(i=0; i<8; i++)
	{
	  if (img->type == I_SLICE)
	    C[k][j][i] = A[temp] << q;
	  D[k][j][i] = B[temp] << q;
	}
}
