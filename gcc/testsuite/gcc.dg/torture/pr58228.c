/* { dg-do run } */

extern void abort (void);
int a[8][8] = {{1}};
int b, c, d, e;

int main ()
{
  for (c = 0; c < 8; c++)
    for (b = 0; b < 2; b++)
      a[b + 4][c] = a[c][0];
  if (a[4][4] != 1)
    abort ();
  return 0;
}
