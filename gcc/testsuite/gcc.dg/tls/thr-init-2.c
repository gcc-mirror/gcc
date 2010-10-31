/* { dg-do run } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-add-options tls } */

extern void abort() ;

static __thread int fstat ;
static __thread int fstat = 1;

int test_code(int b)
{
  fstat += b ;
  return fstat;
}

int main (int ac, char *av[])
{
  int a = test_code(1);
  
  if ((a != 2) || (fstat != 2))
    abort () ;
  
  return 0;
}
