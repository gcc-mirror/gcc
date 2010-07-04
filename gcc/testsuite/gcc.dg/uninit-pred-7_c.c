
/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2" } */

int g;
void bar();
void blah(int);

int foo (int n, int l, int m, int r)
{
  int v;

  if (n)
    v = r;

  if (m) g++;
  else   bar();

  if (n )
    {
      if (l)
        g++;
      else 
        goto l;
    }
  else
    {
l:
      blah (v); /* { dg-warning "uninitialized" "warning" } */
    }

  return 0;
}
