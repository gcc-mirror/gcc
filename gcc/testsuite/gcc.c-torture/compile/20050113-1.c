/* PR c/17297 */
/* { dg-xfail-if "PR target/12916" { "sparc*-*-*" } { "*" } { "" } } */

typedef float V2SF __attribute__ ((vector_size (8)));

int test0 (V2SF, V2SF);

int
main (void)
{
  V2SF a = (V2SF) {1.0f/0.0f - 1.0f/0.0f, 1.0f/0.0f - 1.0f/0.0f};
  V2SF b = (V2SF) {567.345, 1984.0};
  int i;

  i = test0 (a, b);
  return i;
}
