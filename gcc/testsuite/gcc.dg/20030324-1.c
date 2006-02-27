/* { dg-do run } */
/* { dg-options "-O -fstrict-aliasing -fgcse" } */

/* PR optimization/10087 */
/* Contributed by Peter van Hoof <p.van-hoof@qub.ac.uk> */
 
extern void abort(void);

void b(int*,int*);
    
typedef struct {
    double T1;
    char c;
} S;

int main(void)
{
  int i,j;
  double s;

  S x1[2][2];
  S *x[2] = { x1[0], x1[1] };
  S **E = x;

  for( i=0; i < 2; i++ )
    for( j=0; j < 2; j++ )
      E[j][i].T1 = 1;

  for( i=0; i < 2; i++ )
    for( j=0; j < 2; j++ )
      s = E[j][i].T1;

  b(&j,&i);
  if (s != 1)
    abort ();
  return 0;
}

void b(int *i, int *j) {}
