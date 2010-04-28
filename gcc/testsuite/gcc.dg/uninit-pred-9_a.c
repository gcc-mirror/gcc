
/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2" } */

int g;
void bar();
void blah(int);

int foo (int n, int l, int m, int r)
{
  int v;

  if ( (n < 10) && (m == l)  && (r < 20) )
    v = r;

  if (m) g++; 
  else  bar();

  if ( (n <= 8) &&  (m == l)  && (r < 19) )
      blah(v); /* { dg-bogus "uninitialized" "bogus warning" } */

  return 0;
}
