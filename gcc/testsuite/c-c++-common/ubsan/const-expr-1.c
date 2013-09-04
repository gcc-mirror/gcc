/* { dg-do compile } */
/* { dg-options "-fsanitize=shift -w" } */

enum e { A = 1 << 1, B, };
const int arr[] = {
  1 << 2,
  1 << 3,
};

int
bar (int a, int b)
{
  return a >> b;
}

int
foo (void)
{
  int i = 1;
  int vla[B << 3];
  return bar (A, (i <<= 6, i + 2));
}
