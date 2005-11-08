/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize" } */

extern void abort (void);

#define MIN2(a,b) (((a)<(b)) ? (a) : (b))
#define MAX2(a,b) (((a)>(b)) ? (a) : (b))

double p[2] = { 4., 5. };

int main()
{
  long j;
  double R, n, x;

  n = 1.e300;
  x = -1.e300;
  for( j=0; j < 2; j++ )
    {
      x = MAX2(x,p[j]);
      n = MIN2(n,p[j]);
    }
  R = x-n;

  if( R < 0.1 )
      abort ();

  return 0;
}
