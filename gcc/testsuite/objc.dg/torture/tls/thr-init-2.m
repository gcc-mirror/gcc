// { dg-do run }
// { dg-require-effective-target tls_runtime }
// { dg-add-options tls }

extern void _exit(int);

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
    _exit (-(__LINE__)) ;

  a = fa(glb);
  if ( a != 6 ) 
    _exit (-(__LINE__)) ;

  a = fb(a);  
  if ( a != 10 || glb != 10 ) 
    _exit (-(__LINE__)) ;
  
  return 0;
}
