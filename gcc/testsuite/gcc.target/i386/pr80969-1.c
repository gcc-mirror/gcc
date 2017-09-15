/* { dg-do run { target { ! x32 } } } */
/* { dg-options "-Ofast -mabi=ms -mavx512f" } */
/* { dg-require-effective-target avx512f } */

int a[56];
int b;
int main (int argc, char *argv[]) {
  int c;
  for (; b; b++) {
    c = b;
    if (b & 1)
      c = 2;
    a[b] = c;
  }
  return 0;
}
