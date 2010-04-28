/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2" } */

int g;
void bar();
void blah(int);
int foo (int n, int m, int r, int t)
{
  int flag = 0;
  int v;

  if (t)
    {
      if (n)
        {
          v = r;    /* init path 1 */
          flag = 1;
        }

      if (m) g++;
      else bar();

      if (flag)  /* properly  guarded */
        blah(v); /* { dg-bogus "uninitialized" "bogus warning" } */
    }
  else
    {
      v = r+1; /* init path 2 */
      flag = 2;
    }

  if (m) g++;
  else bar();

  if (g)   /* guard can not be determined statically to be safe */
    blah(v); /* { dg-warning "uninitialized" "real warning" } */

  return 0;
}

