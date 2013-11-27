/* { dg-do compile } */

void
baz (int *d)
{
  long int i, j, k;
  for (i = 0, j = 0, k = 0; i < 512; i = (int) i + 1, j = (int) j + 1, k = (int) k + 3)
    d[i] = j ^ (i * 3) ^ (2 * k + 2);
}
