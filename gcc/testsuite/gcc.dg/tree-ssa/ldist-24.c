/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ldist-details --param max-completely-peel-times=8" } */

typedef struct S {
    double z[8][25];
    double x1[8][40];
    double x2[8][40];
    double y[8][35];
} S;

S * par;
void foo ()
{
  int i, j;
  for (i = 0; i<8; i++)
    for (j = 0; j<35; j++)
      {
	par->x1[i][j] = par->x2[i][j];
	par->x2[i][j] = 0.0;
      }
}

/* { dg-final { scan-tree-dump "generated memcpy" "ldist" } } */
/* { dg-final { scan-tree-dump "generated memset zero" "ldist" } } */
