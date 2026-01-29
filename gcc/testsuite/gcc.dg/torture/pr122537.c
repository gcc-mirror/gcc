/* { dg-do run } */
/* { dg-additional-options "-fwrapv" } */

void abort ();
int a, b;
int main()
{
  int c = 61;
  int d = 61;
  c += 8;
  while (c + a - 80 >= d + a - 80) {
    c -= d;
    b++;
  }
  if (b != 1)
    abort ();
  return 0;
}
