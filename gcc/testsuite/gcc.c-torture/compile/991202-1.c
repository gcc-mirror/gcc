extern float A[], B[];
extern float MAXNUMF;
float chbevlf(float, float *, int);
float expf(float), i1f(float), logf(float), sqrtf(float);

float k1f(float xx)
{
  float x, y;

  x = xx;
  if( x <= 2.0 )
    {
      y = x * x - 2.0;
      y =  logf( 0.5f * x ) * i1f(x)  +  chbevlf( y, A, 7 ) / x;
      return( y );
    }
  return(  expf(-x) * chbevlf( (float)(8.0/x - 2.0), B, 10 ) / sqrtf(x) );
}
