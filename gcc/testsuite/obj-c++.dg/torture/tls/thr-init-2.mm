// { dg-do run }
// { dg-require-effective-target tls }
// { dg-add-options tls }

extern "C" {
extern void abort ();
}

__thread int glb =1 ;

static __thread int fstat = 2;

int fa(int a)
{
static __thread int as = 3;
  as += a ;
  return as;
}

int fb(int b)
{
static __thread int bs = 4;
  bs += b ;
  glb = bs;
  return bs;
}

int main (int ac, char *av[])
{
  int a = 1;
  
  a = fa(fstat);
  if ( a != 5 ) 
    abort () ;

  a = fa(glb);
  if ( a != 6 ) 
    abort () ;

  a = fb(a);  
  if ( a != 10 || glb != 10 ) 
    abort () ;
  
  return 0;
}
