/* { dg-require-effective-target alloca } */
/* { dg-additional-options "-fpermissive" } */
int N = 1;
void foo() {} /* Necessary to trigger the original ICE.  */
void bar (char a[2][N]) { a[1][0] = N; }
int
main (void)
{
  void *x;

  N = 4;
  x = alloca (2 * N);
  memset (x, 0, 2 * N);
  bar (x);
  if (N[(char *) x] != N)
    abort ();
  exit (0);
}
