/* PR optimization/10955 */
/* Originator: <heinrich.brand@fujitsu-siemens.com> */

/* This used to fail on SPARC32 at -O3 because the loop unroller
   wrongly thought it could eliminate a pseudo in a loop, while
   the pseudo was used outside the loop.  */

extern void abort(void);

#define COMPLEX struct CS

COMPLEX {
  long x;
  long y;
};


static COMPLEX CCID (COMPLEX x)
{
  COMPLEX a;

  a.x = x.x;
  a.y = x.y;

  return a;
}


static COMPLEX CPOW (COMPLEX x, int y)
{
  COMPLEX a;
  a = x;

  while (--y > 0)
    a=CCID(a);

  return a;
}


static int c5p (COMPLEX x)
{
  COMPLEX a,b;
  a = CPOW (x, 2);
  b = CCID( CPOW(a,2) );
    
  return (b.x == b.y); 
}


int main (void)
{
  COMPLEX  x;  

  x.x = -7;
  x.y = -7;

  if (!c5p(x))
    abort();

  return 0;
}
