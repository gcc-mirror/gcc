/* { dg-do link } */
/* { dg-options "-ffast-math" } */

extern void link_error(void);

void test(double x, int n)
{
  if (__builtin_powi(x,-1.0) != 1.0/x)
    link_error ();
  if (__builtin_powi(x,0.0) != 1.0)
    link_error ();
  if (__builtin_powi(x,1.0) != x)
    link_error ();
  if (__builtin_powi(1.0,n) != 1.0)
    link_error ();
}

int main()
{
  test(7.3, 2);
  return 0;
}

