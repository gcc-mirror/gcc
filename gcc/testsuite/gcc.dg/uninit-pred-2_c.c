/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2 -fno-tree-tail-merge" } */

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

  if (m) g++;
  else bar();

  if (flag)
    blah(v); /* { dg-bogus "uninitialized" "bogus uninitialized var warning" } */ 

  return 0;
}

int foo_2 (int n, int m, int r)
{
  int flag = 0;
  int v; 

  if (n)
    {
      v = r;
      flag = 1;
    }

  if (m) g++;
  else bar();

  if (flag)
    blah(v); /* { dg-bogus "uninitialized" "bogus uninitialized var warning" } */ 
  else
    blah(v); /* { dg-warning "uninitialized" "real uninitialized var warning" } */

  return 0;
}
