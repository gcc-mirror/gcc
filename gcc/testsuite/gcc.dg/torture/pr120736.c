/* { dg-do run } */
/* { dg-additional-options "-fsigned-char -fno-strict-aliasing -fwrapv" } */

unsigned char aa (unsigned char ab, int o) { return ab > o ? ab : 0; }
int p;
int s;
static unsigned char q = 255;
int r;
int *v = &s;
int main() {
  p = v != 0;
  for (; r < 8; ++r) {
    if (s)
      break;
    s = aa(p * q++, 6) <= 0;
  }
  if (q != 1)
    __builtin_abort ();
  __builtin_exit (0);
}

