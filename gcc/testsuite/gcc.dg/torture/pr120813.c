/* { dg-do run } */
/* { dg-additional-options "-fsigned-char -fno-strict-aliasing -fwrapv" } */

short s (short t, short u) { return u == 0 ? 0 : t / u; }
int x[6];
int y;
unsigned ak = 1;
unsigned short al = 65527;
unsigned *am = &ak;
int main() {
  for (int i = 0; i < 6; i++) {
    x[i] = i;
  }
  for (;;) {
    unsigned long ar = 2080554998UL;
    char as = 4;
    if (s(34, al++) < ar)
      if (*am)
        break;
  }
  y = x[al & 5];
  if ((y ^ 5UL) != 4)
    __builtin_abort ();
  __builtin_exit (0);
}


