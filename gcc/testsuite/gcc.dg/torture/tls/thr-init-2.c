/* { dg-do run } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-add-options tls } */

extern int printf (char *,...);
extern void abort() ;

static __thread int fstat ;
static __thread int fstat = 1;
static __thread int fstat ;

int test_code(int b)
{
  fstat += b ;
  return fstat;
}

int main (int ac, char *av[])
{
  int a = test_code(1);
  
  if ( a != 2 || fstat != 2 )
    {
    printf ("a=%d fstat=%d\n", a, fstat) ;
    abort ();
    }
  
  return 0;
}
