/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-expand" } */
void bar( char *);
int foo()
{
  int i=0;
  {
    char a[8000];
    bar(a);
    i += a[0];
  }
  {
    char a[8192];
    char b[32];
    bar(a);
    i += a[0];
    bar(b);
    i += a[0];
  }
  return i;
}
/* { dg-final { scan-rtl-dump "size 8192" "expand" } } */
/* { dg-final { scan-rtl-dump "size 32" "expand" } } */
