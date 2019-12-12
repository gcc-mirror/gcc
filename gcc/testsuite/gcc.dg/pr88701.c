/* PR c/88701 */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

void foo (int [(int (*)[1]) { 0 } == 0]);
void bar (int n, int [(int (*)[n]) { 0 } == 0]);

int
baz (int a[(int (*)[1]) { 0 } == 0])
{
  return a[0];
}

int
qux (int n, int a[(int (*)[n]) { 0 } == 0])
{
  return a[0] + n;
}
