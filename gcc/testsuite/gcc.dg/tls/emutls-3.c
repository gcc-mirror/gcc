/* { dg-do run } */
/* { dg-require-effective-target tls } */
/* { dg-require-effective-target global_constructor } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target tls_runtime } */
/* { dg-add-options tls } */

__thread int i __attribute__((common));

extern void abort (void);

int test_code(int b)
{
  i += b ;
  return i;
}

int main (int ac, char *av[])
{
  int a = test_code(test_code(1));
  
  if ((a != 2) || (i != 2))
    abort () ;
  
  return 0;
}
