/* { dg-do link } */
/* { dg-options "-O" } */

extern void link_error ();
int a[256];
void foo(int n)
{
  int *p;
  for (p = a; n != 0; --n, ++p)
    ;
  if ((__SIZE_TYPE__)p & (__alignof__ (int) - 1))
    link_error ();
}
int main()
{
  return 0;
}
