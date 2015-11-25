/* { dg-do run } */

#define INT_MIN ( -__INT_MAX__ - 1 )

extern void abort (void);

int main (void)
{
  int  x0 = INT_MIN;
  long x1 = 0L;
  int  x2 = 0;
  int  t  = ( 0 || ( INT_MIN - (int) ( x0 - x1 ) ) );

  if ( t != 0 ) { x2 = t; abort(); }

  return 0;
}
