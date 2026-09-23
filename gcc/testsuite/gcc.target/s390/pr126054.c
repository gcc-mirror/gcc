/* { dg-do run { target int128 } } */
/* { dg-options "-O2" } */

int a = 255, b;
long c = 1, d;
long *e = &d;
int main() {
  long f;
  unsigned char g;
  __int128 h = -10;
  f = (long)h * (unsigned)(c | 56);
  for (; b;)
    ;
  g = (unsigned char) f;
  *e = 2540LL ^ g;
  a = 16777215 ^ a ^ d & 255;
  if ((a ^ (int) 4294967295) != 0xFF0000D5)
    __builtin_abort ();
}
