/* PR target/39082 */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2" } */

union un
{
  long double x;
  int i;
};

extern int bar1 (union un);
extern union un bar2 (int);

int
foo1 (union un u)
{
  bar1 (u);
  return u.i;
}

int
foo2 (void)
{
  union un u;
  u.i = 1;
  return foo1 (u) + bar1 (u);
}

int
foo3 (int x)
{
  union un u = bar2 (x); /* { dg-message "note: the ABI of passing union with long double has changed in GCC 4.4" } */
  return u.i;
}
