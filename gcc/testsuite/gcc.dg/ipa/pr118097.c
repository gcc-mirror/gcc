/* { dg-do run { target longlong64 } } */
/* { dg-options "-O2 -fno-inline" } */

int a, b, c, *d = &a;
long long e;
static long long am (long long f, int g) {
  return g == 0 || f == 1 && g == 1 ?: f / g;
}
static void aq (unsigned f)
{
  c ^= e = am(~f, 1);
  b = 7 - (e >= 1) - 33;
  *d = b;
}

int main() {
  //  am(1, 1);
  aq(1);
  if (a == 0xffffffffffffffe5)
    ;
  else
    __builtin_abort();
}
