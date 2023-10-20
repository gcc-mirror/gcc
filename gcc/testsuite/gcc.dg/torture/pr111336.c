/* { dg-do run } */
/* { dg-require-effective-target lp64 } */

extern void abort (void);
int a, b;
long c = 3521733542;
int d[2];
int e(int f, int g) {
  if (f == 0)
    return 0;
  if (f > 200)
    return 0;
  if (g)
    return 5 * f;
  return 0;
}
int main()
{
  int h = 0;
  for (; e((int)c + 773233762, c + 60) + 773163185 + h < 2; h++)
    d[h] = b;
  if (a != 0)
    abort ();
  return 0;
}
