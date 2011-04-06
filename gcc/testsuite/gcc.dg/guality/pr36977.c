/* PR debug/36977 */
/* { dg-do run } */
/* { dg-options "-g" } */
/* { dg-skip-if "" { *-*-* }  { "*" } { "-O0" } } */

void
foo ()
{
}

int
main ()
{
  struct { char c[100]; } cbig;
  struct { int i[800]; } ibig;
  struct { long l[900]; } lbig;
  struct { float f[200]; } fbig;
  struct { double d[300]; } dbig;
  struct { short s[400]; } sbig;

  ibig.i[0] = 55;		/* { dg-final { gdb-test 30 "ibig.i\[0\]" "55" } } */
  ibig.i[100] = 5;		/* { dg-final { gdb-test 30 "ibig.i\[100\]" "5" } } */
  cbig.c[0] = '\0';		/* { dg-final { gdb-test 30 "cbig.c\[0\]" "'\\0'" } } */
  cbig.c[99] = 'A';		/* { dg-final { gdb-test 30 "cbig.c\[99\]" "'A'" } } */
  fbig.f[100] = 11.0;		/* { dg-final { gdb-test 30 "fbig.f\[100\]" "11" } } */
  dbig.d[202] = 9.0;		/* { dg-final { gdb-test 30 "dbig.d\[202\]" "9" } } */
  sbig.s[90] = 255;		/* { dg-final { gdb-test 30 "sbig.s\[90\]" "255" } } */
  lbig.l[333] = 999;		/* { dg-final { gdb-test 30 "lbig.l\[333\]" "999" } } */

  foo ();
  return 0;
}
