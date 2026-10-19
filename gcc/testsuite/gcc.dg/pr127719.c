/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned short a;
unsigned b;
void c() {
  char d = b;
  long e = b + (a & 5ull) & 1;
  if ((unsigned long)b - 2 + e)
    if (e)
      __builtin_unreachable();
  do {
    d >>= e;
    if (d)
      break;
  } while (1);
}
int main() {}
