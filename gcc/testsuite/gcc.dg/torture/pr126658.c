/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

long a, b;
int c, d;
long long e[1];
char __attribute__((noipa))
f(long long *p1, int i, long long *p3)
{
  long long *g;
  for (; a < i; a++)
    g = p1 + 1;
  while (g != p1) {
    --g;
    b = g - p1;
    if (p3[b])
      c = 3;
  }
  return c;
}
int main() {
  long long j[] = {1096435691};
  d = f(e, 2147483647 - 2147279301, j);
  if (d != 3)
    __builtin_abort ();
}
