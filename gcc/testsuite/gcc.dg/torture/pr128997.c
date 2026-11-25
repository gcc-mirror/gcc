/* { dg-do run { target int128 } } */

int j, m;
__int128 g;
signed char v;
int main(void) {
  short x = 0;
  if (!j)
    g = 1 << 16;
  for (; j; j++) {
    v = m || x;
    if (g == v)
      x = v;
  }
  if ((int)(g >> 16) == 0)
    __builtin_abort ();
  __builtin_exit (0);
}

