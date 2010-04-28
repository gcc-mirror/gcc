
/* { dg-do compile } */
/* { dg-options "-Wuninitialized -O2" } */

int g;
void bar();
void blah(int);

int foo (int n, int l, int m, int r)
{
  int v;

  if (n || l)
    v = r;

  if (m) g++;
  else   bar();

  if ( n && l)
      blah(v); /* { dg-bogus "uninitialized" "bogus warning" } */

  if ( n )
      blah(v); /* { dg-bogus "uninitialized" "bogus warning" } */

  if ( l )
      blah(v); /* { dg-bogus "uninitialized" "bogus warning" } */

  return 0;
}

int foo_2 (int n, int l, int m, int r)
{
  int v;

  if (n || l)
    v = r;

  if (m) g++;
  else   bar();

  if ( n && l)
      blah(v); /* { dg-bogus "uninitialized" "bogus warning" } */

  if ( n )
      blah(v); /* { dg-bogus "uninitialized" "bogus warning" } */

  if (m || l)
      blah (v); /* { dg-warning "uninitialized" "warning" } */

  if ( l )
      blah(v); /* { dg-bogus "uninitialized" "bogus warning" } */

  return 0;
}
