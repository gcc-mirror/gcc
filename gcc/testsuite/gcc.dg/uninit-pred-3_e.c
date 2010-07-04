/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2" } */

int g;
void bar();
void blah(int);

int foo (int n, int m, int r)
{
  int flag = 0;
  int v;

  if (n)
    {
      v = r;
      flag = -1;
    }

  if (m)
    g++;
  else 
    bar();

  if (r > 0)
    if (flag  <= 0 )
      blah(v); /* { dg-warning "uninitialized" "real warning" } */
  return 0;
}
