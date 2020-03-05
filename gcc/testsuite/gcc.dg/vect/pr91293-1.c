/* { dg-do run } */
/* { dg-additional-options "-msse4.1" { target { sse4_runtime } } } */

long long a;
unsigned b, c;
int d = 62;
void e(long long *f, int p2) { *f = p2; }
int main()
{
  for (int g = 2; g <= d; g++)
    {
      b += g + 4;
      c += 5 - g;
    }
  e(&a, b);
  if (a != 2196)
    __builtin_abort ();
  return 0;
}
