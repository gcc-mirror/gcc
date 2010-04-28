
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

  if ( n && l)
      blah(v); /* { dg-bogus "uninitialized" "bogus warning" } */

  return 0;
}

