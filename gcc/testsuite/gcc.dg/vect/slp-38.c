/* { dg-do compile } */

typedef struct {
    float l, h;
} tFPinterval;

tFPinterval X[1024];
tFPinterval Y[1024];
tFPinterval Z[1024];

void Compute(void)
{
  int d;
  for (d= 0; d < 1024; d++)
    {
      Y[d].l= X[d].l + X[d].h;
      Y[d].h= Y[d].l;
      Z[d].l= X[d].l;
      Z[d].h= X[d].h;
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 1 "vect" { target { vect_float && vect_perm } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
