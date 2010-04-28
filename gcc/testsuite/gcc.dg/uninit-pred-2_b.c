/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2" } */

int g;
void bar (void);
void blah (int);

int foo (int n, int m, int r)
{
  int flag = 0;
  int v; 

  if (n)
    {
      v = r;
      flag = 1;
    }

  if (m)
    g++;
  else 
    bar();

  /* Wrong guard */
  if (!flag)
    blah(v); /* { dg-warning "uninitialized" "real uninitialized var warning" } */

  return 0;
}
